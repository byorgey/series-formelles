import           Development.Shake
import           Development.Shake.FilePath
import           System.Exit

lhs2TeX, pdflatex, rubber :: String
lhs2TeX  = "lhs2TeX"
pdflatex = "pdflatex"
rubber = "rubber"

main :: IO ()
main = shake shakeOptions $ do

  want ["series-formelles.pdf"]

  -- "*.tex" %> \output -> do
  --     let input = replaceExtension output "lhs"
  --     need [input]
  --     cmd lhs2TeX $ ["-o", output] ++ [input]

  "*.pdf" %> \output -> do
      let input = replaceExtension output "tex"
      need [input]
      cmd pdflatex $ ["--enable-write18", input]
      -- Exit c <- cmd rubber $ ["-f", "-d", input]
      -- case c of
      --   ExitFailure _ -> do
      --     cmd "mklatex" $ ["-pdf", input]
      --   _ -> return ()
