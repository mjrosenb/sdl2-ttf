{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL.TTF as TTF
import qualified SDL     as SDL
import qualified SDL.Raw.Types
import Linear
import Linear.Affine
import Foreign.C.String (withCAString)
import Foreign (peek,alloca,with,maybePeek,nullPtr)

arial :: String
arial = "./examples/ARIAL.TTF"

main :: IO ()
main = do
    _ <- SDL.initialize [SDL.InitVideo]
    window <- createWindow
    renderer <- createRenderer window

    TTF.withInit $ do
      font <- TTF.openFont arial 150 -- Pt size for retina screen. :<
      textSurface <- TTF.renderUTF8Solid font "some text" (SDL.Raw.Types.Color 255 255 255 0)
      textTexture <- SDL.createTextureFromSurface renderer textSurface
      SDL.freeSurface textSurface
      loop window renderer textTexture

      TTF.closeFont font
      SDL.destroyRenderer renderer
      SDL.destroyWindow window
      SDL.quit

createRenderer :: SDL.Window -> IO (SDL.Renderer)
createRenderer w = SDL.createRenderer w (-1) SDL.defaultRenderer

createWindow :: IO (SDL.Window)
createWindow =
      SDL.createWindow "test" (SDL.defaultWindow {SDL.windowSize = V2 640 480})

loop :: t -> SDL.Renderer -> SDL.Texture -> IO ()
loop window renderer textTexture = do
    let loc = SDL.Rectangle (P (V2 320 240)) (V2 150 100)
    SDL.renderClear renderer
    SDL.renderCopy renderer textTexture Nothing (Just loc)
    SDL.renderPresent renderer
    handleEvents window renderer textTexture

handleEvents :: t -> SDL.Renderer -> SDL.Texture -> IO ()
handleEvents window renderer textTexture = do
  mbEvent <- SDL.pollEvent
  case mbEvent of
    Just (SDL.Event _ SDL.QuitEvent) -> return ()
    _ -> loop window renderer textTexture
