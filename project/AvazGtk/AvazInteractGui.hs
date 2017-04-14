module Main () where

import Graphics.UI.Gtk
import Control.Monad.Trans.Class
import Control.Monad
import qualified Control.Lens as L
import Common
import WordPageElem
import Safe
import qualified Data.ByteString.Char8 as DCh8
import qualified Data.Char as C
import qualified Data.IORef as Ref
import qualified Data.Map as M
import qualified Data.Maybe as Mb
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified System.IO as Si
import qualified System.Directory as Dir

noOfWords = 40

data Edits = Edit {gramno::Int,pageno::Int,pos::Int,old::DCh8.ByteString,new::DCh8.ByteString} deriving Show

writeelemto ed pe = pe L.& L.ix (pred.pred.gramno$ed).L.ix (pageno ed).L.ix (pos ed) L..~ (new ed)

writepetofile pr su pe    = do let gram = succ.succ.fst $ pe
                               cwd <- Dir.getCurrentDirectory
                               let filename = cwd++"/data/"++pr++show gram++"-"++show noOfWords++su
                               let flatten x= (DCh8.pack$(show.fst$x)++"\t") `DCh8.append` (snd x)
                               file <- Si.openFile filename Si.WriteMode
                               mapM ((DCh8.hPutStrLn file).flatten) $
                                    zip [0..] (map (DCh8.intercalate (DCh8.pack ",")) (F.toList.snd$pe))
                               Si.hClose file

splitdiv2 :: Int -> (Int,Int)
splitdiv2 n
	| sndh == 0 = (fsth,fsth)
	| remi == 0 = (maxi,mini)
        | otherwise = divuntilrem0 n maxi
        where inpn = toRational.fromIntegral $ n
	      half = properFraction.sqrt.fromRational $ inpn
	      sndh = snd half
	      fsth = fst half
	      remi = n `rem` fsth
	      divi = n `div` fsth
	      maxi = max fsth divi
	      mini = min fsth divi

divuntilrem0 n x
        | x == 1 = (n,x)
	| n `rem` x == 0 = (maxi,mini)
	| otherwise      = divuntilrem0 n $ pred x
	where maxi = max (n `div` x) x
	      mini = min (n `div` x) x

starredTextToBlank x = case x of "**" -> ""
                                 _    -> x

tableLNo = fst $ splitdiv2 noOfWords
tableHNo = snd $ splitdiv2 noOfWords

windowProps c= [ containerBorderWidth := 10
               , containerChild := c
	       , windowTitle := "Avaz Gui"
	       , windowWindowPosition := WinPosCenter
	       , windowResizable := False
	       , widgetWidthRequest := tableLNo*60
	       , widgetHeightRequest := tableHNo*50 ]

dlgProps = [windowTitle := "Edit Entry"
	   ]

buttonAction btn txt btns ct em gp dg eb eds stb = do btndata <- buttonGetLabel $ snd btn
                                                      txtdata <- entryGetText txt
                                                      currentMode <- Ref.readIORef em
                                                      let categItems x = V.fromList.Set.toList.(maybe Set.empty id).M.lookup x $ ct
                                                      case currentMode of
                                                              False-> case btndata of
                                                                      [] -> putStrLn "Real Empty Button"
                                                                      "**" -> putStrLn "Unfilled Empty Button"
                                                                      '*':x -> case C.isUpper $ head x of
                                                                                      True -> do V.mapM_ (flip buttonSetLabel $ "") btns
                                                                                                 V.mapM_ (\(t,btn) -> set btn [ buttonLabel := t]) $
                                                                                                                      V.zip (categItems x) btns
                                                                                      False -> entrySetText txt (unwords.words $ txtdata++" "++x)
                                                                      _  -> case C.isUpper $ head btndata of
                                                                                      True -> do V.mapM_ (flip buttonSetLabel $ "") btns
                                                                                                 V.mapM_ (\(t,btn) -> set btn [ buttonLabel := t]) $
                                                                                                                      V.zip (categItems btndata) btns
                                                                                      False -> entrySetText txt (unwords.words $ txtdata++" "++btndata)
                                                              True -> do  toedit <- buttonGetLabel $ snd btn
                                                                          (gramno,pageno) <- Ref.readIORef gp
                                                                          entrySetText eb toedit
                                                                          widgetShow eb
                                                                          resp <- dialogRun dg
                                                                          case resp of
                                                                                ResponseOk -> do  editedtxt <- entryGetText eb
                                                                                                  let pedited = DCh8.pack editedtxt
                                                                                                  let ptoedit = DCh8.pack toedit
                                                                                                  edits <- Ref.readIORef eds
                                                                                                  let entry = Edit gramno pageno (fst btn) ptoedit pedited
                                                                                                  let towrite = (entry L.<| edits)
                                                                                                  when (editedtxt /= toedit) $
                                                                                                       Ref.writeIORef eds towrite
                                                                                                  buttonSetLabel (snd btn) editedtxt
                                                                                _ -> return ()
                                                                          widgetHide dg

entryChAction txt btns wp per st gp stb = do    txtdata <- entryGetText txt
                                                pe <- Ref.readIORef per
                                                case txtdata of
                                                    []-> do V.mapM_ (flip buttonSetLabel $ "") btns
                                                            V.mapM_ setTxtBtn $ V.zip (V.fromList st) btns
                                                            Ref.writeIORef gp (1,0)
                                                            cid <- statusbarGetContextId stb "PageGram"
                                                            mid <- statusbarPush stb cid "Page: 0 Gram:1"
                                                            return ()
                                                    _ -> do V.mapM_ (flip buttonSetLabel $ "") btns
                                                            (out,page,gramno) <- lookupPage wp pe (DCh8.pack txtdata)
                                                            let val = case out of
                                                                            [] -> map DCh8.pack st
                                                                            _  -> out
                                                            let btnTxt = F.toList $ fmap (DCh8.unpack) $ take noOfWords val
                                                            case out of
                                                                [] -> do  Ref.writeIORef gp (1,0)
                                                                          cid <- statusbarGetContextId stb "PageGram"
                                                                          mid <- statusbarPush stb cid "Page: 0 Gram:1"
                                                                          return ()
                                                                _ -> do Ref.writeIORef gp (gramno,page)
                                                                        cid <- statusbarGetContextId stb "PageGram"
                                                                        mid <- statusbarPush stb cid $
                                                                               "Page: "++show page++" Gram:"++show gramno
                                                                        return ()
                                                            V.mapM_ setTxtBtn $ V.zip (V.fromList btnTxt) btns

setTxtBtn (t,btn) = set btn [ buttonLabel := starredTextToBlank t
                             ,widgetWidthRequest := 50
			     ,widgetHeightRequest := 25
			     ]

xrem x = x `rem` tableLNo
xdiv x = x `div` tableLNo
xrems = succ.xrem
xdivs = succ.xdiv

attachToTable table buttons x = tableAttach table (buttons V.! x) (xrem x) (xrems x) (xdiv x) (xdivs x) [Fill] [Fill] 5 5

ui st ct wp per = do	editMode <- Ref.newIORef False
                        grampage <- Ref.newIORef (1,0)
                        edits <- Ref.newIORef []
                        window <- windowNew
			elembuttons <- V.replicateM noOfWords buttonNew
			editWindow <- dialogNew
			clastBtn <- buttonNew
			clearBtn <- buttonNew
			homeBtn <- buttonNew
			editBtn <- buttonNew
			writeBtn <- buttonNew
			textbox <- entryNew
			editbtnbox <- entryNew
			table <- tableNew tableHNo tableLNo True
			centerAlignbtns <- alignmentNew 0.5 0.5 1 1
			tableAttachDefaults table centerAlignbtns 0 1 0 1
			mapM_ (attachToTable table elembuttons) [0..pred noOfWords]
			vsplit <- vBoxNew False 5
			uhsplit <- hBoxNew False 5
			dhsplit <- hButtonBoxNew
                        stb <- statusbarNew
			set textbox [entryText := ""
			            ,widgetWidthRequest := tableLNo*60
				    ,widgetHeightRequest := 30
				    ]
			V.mapM_ setTxtBtn $ V.zip (V.fromList st) elembuttons
			set clearBtn [buttonLabel := "Clear"
			             ,widgetWidthRequest := 25
				     ,widgetHeightRequest := 25
				     ]
			set clastBtn [buttonLabel := "Back"
			             ,widgetWidthRequest := 25
				     ,widgetHeightRequest := 25
				     ]
			set homeBtn [buttonLabel := "Home"
			            ,widgetWidthRequest := 25
				    ,widgetHeightRequest := 25
				    ]
			set editBtn [buttonLabel := "Edit"
			            ,widgetWidthRequest := 25
				    ,widgetHeightRequest := 25
				    ]
			set writeBtn [buttonLabel := "Write"
			             ,widgetWidthRequest := 25
				     ,widgetHeightRequest := 25
				     ]
			set uhsplit [containerChild := homeBtn
			            ,containerChild := textbox
			            ,containerChild := clastBtn
			 	    ,containerChild := clearBtn
				    ,boxChildPacking homeBtn := PackNatural
				    ,boxChildPacking clastBtn := PackNatural
				    ,boxChildPacking clearBtn := PackNatural
				    ]
			set dhsplit [containerChild := editBtn
			            ,containerChild := writeBtn
			            ,boxChildPacking editBtn := PackNatural
				    ,buttonBoxLayoutStyle := ButtonboxCenter
				    ]
			set vsplit [containerChild := uhsplit
			           ,containerChild := table
				   ,containerChild := dhsplit
                                   ,containerChild := stb
				   ]
			set window $ windowProps vsplit
			set editWindow dlgProps
			dialogAddActionWidget editWindow editbtnbox ResponseOk
			windowSetGeometryHints window (Just table) Nothing Nothing Nothing Nothing Nothing
                        let applButtonAction x = buttonAction x textbox elembuttons ct editMode grampage editWindow editbtnbox edits stb
			V.mapM_ (\x -> on (snd x) buttonActivated $ applButtonAction x) $ V.indexed elembuttons
			on textbox editableChanged $ entryChAction textbox elembuttons wp per st grampage stb
			on textbox entryActivated  $ entryChAction textbox elembuttons wp per st grampage stb
			on clearBtn buttonActivated $ do entrySetText textbox ""
			on clastBtn buttonActivated $ do txtdat <- entryGetText textbox
			                                 let outtxt = unwords.initDef [""].words $ txtdat
			                                 entrySetText textbox outtxt
			on homeBtn buttonActivated $ do V.mapM_ (flip buttonSetLabel $ "") elembuttons
					                V.mapM_ setTxtBtn $ V.zip (V.fromList st) elembuttons
			on editBtn buttonActivated $ do currentMode <- Ref.readIORef editMode
			                                let switchto = if currentMode then "Edit" else "Test"
							buttonSetLabel editBtn switchto
			                                Ref.writeIORef editMode $ not currentMode
			on writeBtn buttonActivated $ do eds <- Ref.readIORef edits
                                                         pe <- Ref.readIORef per
                                                         let finpe = foldr writeelemto pe eds
                                                         let pref = "pageelems"
                                                         let suff = "edited.txt"
                                                         putStrLn "Writing changes to memory really and file..."
                                                         Ref.writeIORef per finpe
                                                         mapM_ (writepetofile pref suff) $ zip [0..] finpe
			after window deleteEvent $ tryEvent $ do lift $ widgetHide window
							         lift $ mainQuit
			widgetShowAll window

main = do initGUI
	  wpMap <- wordPageIdMap
	  peVec <- pageIdElemsVec
	  peRef <- Ref.newIORef peVec
	  ngcat <- ngCategMap
	  startText <- ngStart
          ui startText ngcat wpMap peRef
          mainGUI
