
import           Development.Shake
import           Development.Shake.FilePath
import           System.Exit

lhs2TeX, pdflatex, rubber, agda :: String
lhs2TeX  = "lhs2TeX"
pdflatex = "pdflatex"
rubber   = "rubber"
agda     = "agda"

main :: IO ()
main = shake shakeOptions $ do

  want ["series-formelles.pdf"]

  "latex/*.tex" %> \output -> do
      let input = takeFileName output -<.> "agda"
      need [input]
      cmd agda $ ["--latex", input]

  "*.tex" %> \output -> do
      let input = output -<.> "lhs"
      need [input]
      cmd lhs2TeX $ ["-o", output, input]

  "*.pdf" %> \output -> do
      let input = replaceExtension output "tex"

      need [input]
      cmd pdflatex $ ["--enable-write18", input]
