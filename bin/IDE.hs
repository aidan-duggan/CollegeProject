
module Main where
import Graphics.UI.WXCore
import Graphics.UI.WX
import System
import System.IO
import System.Process 
import System.Directory
import Data.Maybe
import Control.Exception
--Start text which set the ground work around which the user can build their application
setupText = "data UserRecord = UserRecord{\n    screenX :: Int,\n    screenY :: Int\n}\n\ndefaultUserRecord = UserRecord 200 200\n\nsetup :: UserRecord -> Quick UserRecord\nsetup record = do\n                        --Do Setup Here\n                        return record\n\npainting :: DC a -> UserRecord -> Quick UserRecord\npainting dc record = do\n                        --Generate Graphics Here\n                        return record"

--the main function
main = do start editorFrame
--Create the UI
editorFrame :: IO ()
editorFrame = do 
				--Create some variables for store infomation like file and directory location
				processInfo <- varCreate []
				fileLoc <- varCreate "DefaultName.hg"
				dirLoc <- varCreate ""
				a <- getCurrentDirectory
				varSet dirLoc a
				f <- frame [text := "Editor"]
				p <- panel f []
				--Create the text box for editing and the text box for compiler output
				txt <- textCtrlRich p [text := setupText, size := Size 500 425, position := pt 0 25 , wrap := WrapNone]
				--No tab support because of strict indentation requirments, function to make indentation uniform added
				set txt [ on enterKey := indentText txt]
				output <- textCtrlRich p [bgcolor := black, textColor := red, size := Size 500 130, position := pt 0 450]
				textCtrlSetEditable output False
				set f [layout := column 1 [widget p], size := Size 480 480]
				--Create the buttons for running the program, saving/loading and quit
				r <- button p [text := "Run", size := Size 100 25, position := pt 0 0, on command := compileAndRun txt f processInfo output dirLoc fileLoc]
				s <- button p [text := "Save", size := Size 100 25, position := pt 100 0, on command := saveProFile f fileLoc txt]
				l <- button p [text := "Load", size := Size 100 25, position := pt 200 0, on command := openProFile f fileLoc txt]
				q <- button p [text := "Quit", size := Size 100 25, position := pt 300 0, on command := close f]
				return ()
--Luanch a file selection dialog
openProFile fra fileL txt = do
				file <- fileOpenDialog fra True False "Load Program" [("Haskell File",["*.hg"]), ("Any File",["*.*"])] "" ""
				--If a file is selected load the text.
				if(isJust file)
					then do
						varSet fileL (fromJust file)
						h1 <- openFile (fromJust file) ReadMode
						t <- hGetContents h1
						set txt [text := t]
						hClose h1
						set fra [text := ("Editor: " ++ (fromJust file))]
					else return()

saveProFile fra fileL txt = do
							fileLo <- varGet fileL
							file <- fileSaveDialog fra True True "Save Program" [("Haskell File",["*.hg"]), ("Any File",["*.*"])] "" fileLo
							if(isJust file)
								then do
									varSet fileL (fromJust file)
									a <- get txt text
									writeFile (fromJust file) a
								else return ()


						
					
compileAndRun :: (Textual w, Textual w1) => w -> Window a -> Var [([Char] -> IO StreamStatus, Process (), Int, Bool)] -> w1 -> Var [Char] -> Var FilePath -> IO ()
compileAndRun txt fra processInfo output fileDir fileLoc= do 
				--Get the file and directory info
				dir <- varGet fileDir
				file <- varGet fileLoc
				--If the directory and file have been set
				if(dir /= "" && file /= "")
					then do
						--Save the text to file
						a <- get txt text
						writeFile (file) a
						h1 <- openFile (dir ++ "\\bin\\library.hs") ReadMode
						h2 <- openFile (dir ++ "\\bin\\result.hs") WriteMode
						h3 <- openFile (file) ReadMode
						--merge the user file with the library and place a temp file called result
						copyFilePro h1 h2
						hClose h1
						copyFilePro h3 h2
						hClose h2
						hClose h3
						--Launch the command line to compile and run the program
						startProcess fra ("ghc --make " ++ dir ++ "\\bin\\result.hs") processInfo output dir
						return ()
					else
						return ()

--Used to make indentation easier by copying the indentation of the previous line
indentText :: TextCtrl a -> IO ()
indentText input = do
					--Find where the cursor is in characters and how long the text is in characters
					ins <- textCtrlGetInsertionPoint input
					end <- textCtrlGetLastPosition input
					--Get the text
					txt <- get input text
					--reverse the text and remove all the characters up until the cursor position and then return the order to the orginial
					txt <- return (drop ((end - ins)) (reverse txt))
					--Count the number of spaces in the indentation of the line behind the cursor
					spaces <- return (findNoSpaces txt)
					--If there is a space add it the current position to bring it inline
					if(spaces > 0)
						then textCtrlWriteText input (replicateFirstCharOfList " " (spaces-1))
						else return ()
--Finds the number of spaces from the last non-space character to the first newline character
findNoSpaces :: [Char] -> Int
findNoSpaces [] = 0
findNoSpaces (t:ts) = countSpaces ts 0 

countSpaces :: [Char] ->Int -> Int
countSpaces [] i = 0
countSpaces (t:ts) i = 
					if(t == ' ')
						--Begin counting characters
						then countSpaces ts (i+1) 
						else if (t == '\n')
							--return result on find a newline else restart the count
							then i
							else countSpaces ts 0
							
--Replicates  the first character of a list the given number of times
replicateFirstCharOfList :: [Char] -> Int -> [Char]
replicateFirstCharOfList x 0 = x
replicateFirstCharOfList x n = replicateFirstCharOfList ((head x):x) (n-1)

--Compiles and runs the program handling any output
startProcess :: (Textual w) => Window a -> [Char] -> Var [([Char] -> IO StreamStatus, Process (), Int, Bool)] -> w -> [Char] -> IO ()
startProcess f action processInfo output dir = do
											set output [text := ""]
											(send,process,pid) <- processExecAsyncTimed f "cmd" True (onEndProcess processInfo output) (onReceive processInfo output dir) (onReceive processInfo output dir)
											--Store the process information so the callbacks can control the command line
											varSet processInfo [(send,process,pid,False)]
											send (action ++ "\n")
											return ()
											
--When the command line is terminated simply print done
onEndProcess :: (Textual w) => Var [([Char] -> IO StreamStatus, Process (), Int, Bool)] -> w -> t -> IO ()
onEndProcess processInfo output code = do appendText output ("\nDone")

--Parses the text being output from the command line, check for confirmation of compilation before launching the program
onReceive :: (Textual w) => Var [([Char] -> IO StreamStatus, Process (), Int, Bool)] -> w -> [Char] -> String -> t -> IO ()
onReceive processInfo output dir txt ss = do
										appendText output txt
										a <- varGet processInfo
										(send,process,pid,check) <- return (head a)
										--Check if the file compiled successfully
										if(txt == "Linking " ++ dir ++ "\\bin\\result.exe ...\n")
											then varSet processInfo [(send,process,pid,True)]
											--Check that the prompt has appeared and if the code compile if so launch the .exe
											else if(txt == dir ++ ">" && check == True)
													then do
														send (dir ++ "\\bin\\result.exe" ++ "\n")
														varSet processInfo [(send,process,pid,False)]
														return ()
													--If a prompt is found and the code did not compile or has ran and been closed then kill the command line
													else if (txt == dir ++ ">" && check == False)
															then do
																unitIO (kill pid wxSIGKILL)
															else return ()

--Copies one file to the other
copyFilePro :: Handle -> Handle -> IO ()
copyFilePro h1 h2 = do
					eof <- hIsEOF h1
					if eof then return () else
					  do
						c <- hGetChar h1
						hPutChar h2 c   
						copyFilePro h1 h2
						
{- Need to be able to print out compiler errors, therefore need to read from command, 
attempt 1 spawn process that compilers attempt to read output to handle.
Failed, can't generate process for some unknown reason therefore can't read in output
attempt 2 spawn command low level OS commandline tool, can us to compile but output sent to main command line
theory on 2 indeed on removing commandline anyway could out put be feedback into program so as to capture attempt 2 results
theory on 2 need to check if compiled successfully before allowing executable to run.

Need to run compiled code, createProcess will successfully run executables 
however now need to print output concurrently with program, will require this whole section to be threaded
				-}