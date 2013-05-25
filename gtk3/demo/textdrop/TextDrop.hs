import Control.Monad.IO.Class
import Graphics.UI.Gtk

main = do
	initGUI
	w <- windowNew
	l <- labelNew $ Just "drag here lol"
	onDestroy w mainQuit
	containerAdd w l
	dragDestSet w [DestDefaultMotion, DestDefaultDrop] [ActionCopy]
	dragDestAddTextTargets w
	w `on` dragDataReceived $ \dc pos id ts -> do
		s <- selectionDataGetText
		liftIO . putStrLn $ case s of
			Nothing -> "didn't understand the drop"
			Just s  -> "understood, here it is: <" ++ s ++ ">"
	widgetShowAll w
	mainGUI
