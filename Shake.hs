
import           Development.Shake
import           Development.Shake.FilePath
import           System.Exit

lhs2TeX, pdflatex, rubber, agda, bibtex :: String
lhs2TeX  = "lhs2TeX"
pdflatex = "pdflatex"
rubber   = "rubber"
agda     = "agda"
bibtex   = "bibtex"

main :: IO ()
main = shake shakeOptions $ do

  want ["series-formelles.pdf"]

  "latex/*.tex" %> \output -> do
      let input = takeFileName output -<.> "lagda"
      need [input]
      cmd agda $ ["--latex", input]

  "*.tex" %> \output -> do
      let input = output -<.> "lhs"
      need [input]
      cmd lhs2TeX $ ["-o", output, input]

  "*.bbl" %> \output -> do
      let input = output -<.> "bib"
      need [input]
      cmd bibtex $ [dropExtension input]

  "*.pdf" %> \output -> do
      let input = output -<.> "tex"
      agdaFiles <- getDirectoryFiles "" ["*.lagda"]
      need (map (\f -> "latex" </> (f -<.> "tex")) agdaFiles)
      need [input, output -<.> "bbl"]
      cmd pdflatex $ ["--enable-write18", input]
