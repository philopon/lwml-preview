{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Conduit as HC

import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WS

import qualified Filesystem as P
import qualified Filesystem.Path as P
import qualified Filesystem.Path.Rules as P
import qualified Filesystem.Path.CurrentOS as PC
import qualified System.FSNotify as FSNotify
import System.Environment(getProgName)

import Control.Concurrent
import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict

import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time.Clock
import Data.Time.Clock.POSIX

import qualified Text.Pandoc as Pandoc
import Text.Blaze.Renderer.Utf8

import Options.Applicative

import qualified Text.Hamlet as Hamlet
import qualified Text.Julius as Julius

import qualified Data.Aeson as Json

import Data.Reflection
import Data.List

import qualified Text.Highlighting.Kate        as Kate (Style, styleToCss)
import qualified Text.Highlighting.Kate.Styles as Kate

import qualified Language.Haskell.TH as TH

urlRule :: P.Rules SC.ByteString
urlRule = P.posix

application :: Given Engine => Maybe P.FilePath -> P.FilePath -> SC.ByteString -> SC.ByteString -> Int -> Wai.Application
application mbCssFile file js css porti req = case Wai.rawPathInfo req of
    "/" -> do progName <- getProgName
              return $ Wai.responseLBS HTTP.status200 [("Content-Type", "text/html")] 
                  (renderMarkup $ $(Hamlet.hamletFile "template/index.hamlet") ())
    path | path == js  -> let port = Json.Number $ fromIntegral porti
                              cont = Julius.renderJavascriptUrl (\_ _ -> "")
                                     $(Julius.juliusFile "template/default.julius")
                          in return $ Wai.responseLBS HTTP.status200 
                             [("Content-Type", "text/javascript")] (TL.encodeUtf8 cont)
         | path == css -> case (mbCssFile, given) of
             (Just cssFile, _) -> return $ Wai.responseFile HTTP.status200 [("Content-Type", "text/css")] (PC.encodeString cssFile) Nothing
             (_, Pandoc{pandocStyle = style}) -> return $ Wai.responseLBS HTTP.status200
                 [("Content-Type", "text/css")] (TL.encodeUtf8 . TL.pack $ Kate.styleToCss style)
             (_, Github) -> return $ Wai.responseLBS HTTP.status200
                 [("Content-Type", "text/css")] $(TH.litE . TH.stringL =<< TH.runIO (readFile "2554919/github.css"))
         | otherwise   -> do
             let filePath = P.directory file P.</> P.decode urlRule (SC.tail path)
             e <- P.isFile filePath
             if e
                 then return $ Wai.responseFile HTTP.status200 [] (PC.encodeString filePath) Nothing
                 else return $ Wai.responseLBS HTTP.status404 [] "404 page not found."


serverApp :: Given Engine => MVar Int -> MVar [(Int, WS.Connection)] -> P.FilePath -> WS.ServerApp
serverApp serial clients file = \pc -> do
    conn <- WS.acceptRequest pc
    ser  <- modifyMVar serial (\s -> (succ s,s) <$ modifyMVar_ clients (return . (:) (s, conn)))
    html <- readHTML file
    WS.sendDataMessage conn (WS.Text html)
    forever $ 
        do void $ WS.receiveDataMessage conn
        `catch` (\(_::WS.ConnectionException) -> modifyMVar_ clients (return . filter ((==) ser . fst)))

run :: Given Engine => Maybe P.FilePath -> Int -> P.FilePath -> NominalDiffTime -> IO ()
run customCss port file refractory = FSNotify.withManager $ \mgr -> do
    clients <- newMVar []
    serial  <- newMVar (0 :: Int)
    fsnotif <- newChan

    FSNotify.watchTreeChan mgr (P.directory file) (const True) fsnotif

    tid <- forkIO . void . flip runStateT (posixSecondsToUTCTime 0) . forever $ do
        lasttime <- get
        notif    <- liftIO $ readChan fsnotif
        when (FSNotify.eventTime notif `diffUTCTime` lasttime > refractory) $ do
            put (FSNotify.eventTime notif)
            html <- liftIO $ readHTML file
            liftIO . modifyMVar_ clients $ filterM (\(_,c) ->
                (True <$ WS.sendDataMessage c (WS.Text html))
                `catch` (\ThreadKilled       -> return False)
                `catch` (\(e::SomeException) -> print e >> return False)
                )

    let name ext = SC.cons '/' . P.encode urlRule . P.filename <$> 
                   uniqueFileName (\n -> P.directory file P.</> P.decodeString urlRule n P.<.> ext)
    js  <- name "js"
    css <- name "css"

    Warp.run port (WS.websocketsOr WS.defaultConnectionOptions 
        (serverApp serial clients file) (application customCss file js css port))
        `finally` (killThread tid)

readPandoc :: Pandoc.ReaderOptions -> P.FilePath -> IO Pandoc.Pandoc
readPandoc conf file = 
    let func = case P.extension file of
                ext | rst ext     -> Pandoc.readRST
                    | tex ext     -> Pandoc.readLaTeX
                    | textile ext -> Pandoc.readTextile
                    | docbook ext -> Pandoc.readDocBook
                    | otherwise   -> Pandoc.readMarkdown
    in func conf <$> (readFile $ PC.encodeString file)
  where rst     = (== Just "rst")
        tex     = (`elem` map Just ["tex", "latex"])
        textile = (== Just "textile")
        docbook = (== Just "docbook")


readGithub :: P.FilePath -> IO LC.ByteString
readGithub file = do
    text <- T.readFile (PC.encodeString file)
    prog <- getProgName
    let query = Json.encode $ Json.object ["text" Json..= Json.String text, "mode" Json..= Json.String "markdown"]
    request <- HC.parseUrl "https://api.github.com/markdown"
    HC.withManager $
        fmap HC.responseBody . HC.httpLbs request
            { HC.method = HTTP.methodPost
            , HC.requestBody = HC.RequestBodyLBS query
            , HC.requestHeaders = ("User-Agent", SC.pack prog) : HC.requestHeaders request
            }

readHTML :: Given Engine => P.FilePath -> IO LC.ByteString
readHTML file = case given of
    Pandoc{pandocFunction = Nothing} -> fmap pandocToHTML $ readPandoc Pandoc.def file
    Pandoc{pandocFunction = Just f } -> fmap pandocToHTML $ readFile (PC.encodeString file) >>= f Pandoc.def
    Github                           -> readGithub file
  where pandocToHTML = renderMarkup . Pandoc.writeHtml Pandoc.def { Pandoc.writerHighlight = True }

data Options = Options { optionPort   :: Int
                       , optionFile   :: P.FilePath
                       , optionCss    :: Maybe P.FilePath
                       , optionEngine :: Engine
                       }

data Engine = Pandoc { pandocFunction :: Maybe (Pandoc.ReaderOptions -> String -> IO Pandoc.Pandoc)
                     , pandocStyle    :: Kate.Style
                     }
            | Github

uniqueFileName :: (String -> P.FilePath) -> IO P.FilePath
uniqueFileName namef = go (0 :: Int)
  where exists f = (||) <$> P.isFile f <*> P.isDirectory f
        go i = let name = namef (show i)
               in exists name >>= \e -> if e then go (succ i) else return name

styles :: [(String, Kate.Style)]
styles = [ ("pygments",   Kate.pygments)
         , ("kate",       Kate.kate)
         , ("espresso",   Kate.espresso)
         , ("tango",      Kate.tango)
         , ("haddock",    Kate.haddock)
         , ("monochrome", Kate.monochrome)
         , ("zenburn",    Kate.zenburn)
         ]

main :: IO ()
main = do
    options <- execParser pinfo
    putStrLn $ "http://localhost:" ++ show (optionPort options)
    give (optionEngine options) $ run (optionCss options) (optionPort options) (optionFile options) 1
  where 
    pinfo  = info (helper <*> parser) fullDesc
    parser = Options
             <$> option (long "port" <> short 'p' <> metavar "PORT" <> help "port number" <> showDefault <> value 8000)
             <*> argument (Just . PC.decodeString) (metavar "FILE")
             <*> nullOption (long "css" <> short 'c' <> metavar "FILE" <> help "custom css file" <> value Nothing <> reader (return . Just . PC.decodeString))
             <*> (engine <*> typeOption <*> styleOption)
    engine = (flag' Pandoc (long "pandoc" <> help "use pandoc engine (default)"))   <|>
              flag' (\_ _ -> Github) (short 'g' <> long "github" <> help "use github engine") <|>
              pure Pandoc
    typeOption   = nullOption (short 'i' <> long "input" <> reader typeReader <> value Nothing <> help "input type of pandoc" <> metavar "TYPE")
    styleOption  = nullOption (short 's' <> long "style" <> reader styleReader <> value Kate.kate <> help "syntax highlight of pandoc" <> metavar "STYLE")
    typeReader  s = case lookup s Pandoc.readers of
        Nothing -> fail $ "unknown input type. one of " ++ intercalate ", " (map fst Pandoc.readers) ++ "."
        Just f  -> return (Just f)
    styleReader s = case lookup s styles of
        Nothing -> fail $ "unknown style. one of " ++ intercalate ", " (map fst styles) ++ "."
        Just f  -> return f

