{-# LANGUAGE OverloadedStrings #-}
module Text.GHCi.Haddock where

import Documentation.Haddock.Markup
import Documentation.Haddock.Parser
import Documentation.Haddock.Types

import Data.Text.Prettyprint.Doc

import Data.Text.Prettyprint.Doc.Render.Terminal

import Data.String
import Data.Void
import Data.Char (isSpace)
import Control.Monad (join)

printDoc :: String -> IO ()
printDoc = putDoc . renderDocH defaultHaddockPrintConf . _doc . parseParas Nothing


defaultHaddockPrintConf :: HaddockPrintConf
defaultHaddockPrintConf = HaddockPrintConf
  { hpc_emphasis = italicized
  , hpc_bold = bold
  , hpc_monospaced = color Magenta 
  , hpc_header = color White 
  , hpc_identifier = underlined <> color Magenta
  , hpc_math = color Green
  , hpc_links = underlined <> color White 
  , hpc_control = color Red
  }

data HaddockPrintConf = HaddockPrintConf
  { hpc_emphasis :: AnsiStyle
  -- ^ emphasized text

  , hpc_bold :: AnsiStyle
  -- ^ bold test

  , hpc_monospaced :: AnsiStyle
  -- ^ code blocks and inline code

  , hpc_header :: AnsiStyle
  -- ^ header bodies

  , hpc_identifier :: AnsiStyle
  -- ^ identifiers, module links, anchors

  , hpc_math :: AnsiStyle
  -- ^ @\\( ... \\)@ and @\\[ ... \\]@ delimited math
  
  , hpc_links :: AnsiStyle
  -- ^ the link part of hyperlinks or images
  
  , hpc_control :: AnsiStyle
  -- ^ things like: equals in headers, bullets in lists, etc.
  }

renderDocH
  :: HaddockPrintConf
  -> DocH Void Identifier
  -> Doc AnsiStyle
renderDocH hpc = annotate (color Green) . (<> hardline) . vcat . punctuate hardline . renderBlocks
  where
  blocksToDoc :: [Doc AnsiStyle] -> Doc AnsiStyle
  blocksToDoc = align . vcat . punctuate hardline

  renderBlocks b = case b of
    DocEmpty -> []
    DocAppend d1 d2 -> renderBlocks d1 ++ renderBlocks d2
    
    DocParagraph p -> [renderInline p False]
    DocWarning d -> [annotate (italicized <> color Red) (renderInline d False)] 
    DocMathDisplay s -> [math ("\\[" <> fromString s <> "\\]")]

    DocUnorderedList ds -> renderListLike (repeat "*")                        ds 
    DocOrderedList   ds -> renderListLike [ fromString (show i) <> "." 
                                          | i <- [1 ..] ] ds
   
    DocDefList ds -> pure $ blocksToDoc $
      [ link lbl' <> ctrl ":" <> hardline <> indent 4 d'
      | (lbl, d) <- ds
      , let lbl' = renderInline lbl False
      , let d' = renderInline d False -- blocksToDoc (renderBlocks d)
      ]
    
    DocHeader (Header lvl title) ->
      let equals = ctrl (fromString (replicate lvl '='))
      in [equals <+> annotate (hpc_header hpc) (renderInline title False)]
    DocTable _ -> error "todo"
    
    DocCodeBlock d -> [annotate (color White) $ mono (vcat [ "@", renderInline d True, "@" ])]

    -- This shouldn't happen. If it does, interpreting the inline markup as
    -- one block seems reasonable
    i -> [renderInline i False]

  -- Second argument being true means "don't reflow my spaces!"
  renderInline i sp = case i of
    DocEmpty -> mempty
    DocAppend d1 d2 -> renderInline d1 sp <> renderInline d2 sp
    
    DocString "" -> mempty
    DocString s
      | not sp  -> let headSpace = if isSpace (head s) then space else mempty
                       lastSpace = if isSpace (last s) then space else mempty
                   in headSpace <> fillSep (map fromString (words s)) <> lastSpace
      | otherwise -> fromString s
    
    DocIdentifier (_,ident,_) -> annotate (hpc_identifier hpc) (fromString ident)
    DocModule     mdl         -> annotate (hpc_identifier hpc) (fromString mdl)
    DocAName      anc         -> annotate (hpc_identifier hpc) (fromString anc)
    DocIdentifierUnchecked v  -> absurd v
    
    DocEmphasis   d -> annotate (hpc_emphasis hpc) (renderInline d sp)
    DocMonospaced d -> mono ("@" <> renderInline d sp <> "@")
    DocBold       d -> annotate (hpc_bold hpc) (renderInline d sp)

    DocMathInline s -> math ("\\(" <+> fillSep (map fromString (words s)) <+> "\\)")
    
    DocCodeBlock d -> mono ("@" <> renderInline d sp <> "@")
    
    DocHyperlink (Hyperlink uri Nothing) -> ctrl "<" <> link (fromString uri) <> ctrl ">"
    DocHyperlink (Hyperlink uri (Just d)) ->
      mconcat [ ctrl "["
              , fromString d -- renderInline d 
              , ctrl "]("
              , link (fromString uri)
              , ctrl ")" ]
      
    DocPic (Picture uri Nothing) -> ctrl "<<" <> link (fromString uri) <> ctrl ">>"
    DocPic (Picture uri (Just s)) ->
      mconcat [ ctrl "!["
              , fromString s 
              , ctrl "]("
              , link (fromString uri)
              , ctrl ")" ]
    
    DocProperty s -> ctrl "prop>" <> fromString s
    DocExamples exs -> vcat (join [ (ctrl ">>>" <+> fromString exp) :
                                    (map (mono . fromString) out)
                                  | Example exp out <- exs ])

    -- This shouldn't happen. If it does, interpreting and flattening the block
    -- markup seems reasonable
    d -> blocksToDoc (renderBlocks d)

  renderListLike ixs ds =
    let bs = map (blocksToDoc . renderBlocks) ds
        bs' = zipWith (\ix d -> annotate (hpc_control hpc) ix <+> d) ixs bs
    in [ indent 4 (blocksToDoc bs') ]


  ctrl = annotate (hpc_control hpc)
  link = annotate (hpc_links hpc)
  math = annotate (hpc_math hpc)
  mono = annotate (hpc_monospaced hpc)

sampleDoc1 = unlines $
  [ "Hellow world this is a really [google](www.google.com) really really long paragraph which is just not ever end \\(1 + 2 \\) at allllllllllllllllll like ever ever"
  , ""
  , "  * bullet 1"
  , "  * bullet 2"
  , "  * bullet 3"
  , "  * bullet 4"
  , ""
  , "and then..."
  ]

sampleDoc2 = unlines $
  [ "= The best section"
  , ""
  , "Hellow world this is a really really really long paragraph which is just not ever end at allllllllllllllllll like ever ever"
  , ""
  , "  * bullet 1"
  , "  * bullet 2 which is sort of never /ending/ like ever e ever e ever e ever e ever e ever e ever e ever e ever e ever e ever  "
  , "    bullet 2 which is sort of never ending like ever e ever e ever e ever e ever e ever e ever e ever e ever e ever e ever  "
  , ""
  , "        - subpoint 1"
  , "        - subpoint 2"
  , ""
  , "      bullet 2 which is sort of never ending like ever e ever e ever e ever e ever e ever e ever e ever e ever e ever e ever  "
  , "      bullet 2 which is sort of never ending like ever e ever e ever e ever e ever e ever e ever e ever e ever e ever e ever  "
  , ""
  , "  * bullet 3"
  , ""
  , "      >>> f x"
  , "      SomeConstructor [1,2,3]"
  , "      >>> f x y z"
  , "      SomeConstructor {"
  , "        x = 019343298472"
  , "        y = fdsifsiudfyoisudf"
  , "      }"
  , ""
  , "      @"
  , "      fib 0 = 0"
  , "      fib 1 = 1"
  , "      fib n = 'fib' (n-1) + fib (n-2)"
  , "      @"
  , ""
  , "      or"
  , ""
  , "      > fib 0 = 0"
  , "      > fib 1 = 1"
  , "      > fib n = fib (n-1) + fib (n-2)"
  , ""
  , "  * bullet 4"
  , ""
  , "and then..."
  , ""
  , "@"
  , "fib 0 = 0"
  , "fib 1 = 1"
  , "fib n = 'fib' (n-1) + fib (n-2)"
  , "@"
  , ""
  , "or"
  , ""
  , "> fib 0 = 0"
  , "> fib 1 = 1"
  , "> fib n = fib (n-1) + fib (n-2)"
  , ""
  , "=== HEADER 2"
  , ""
  , "\\["
  , "\\int_0^\\infty e^{-x^2} dx"
  , "\\]"
  , ""
  , "And here is an example:"
  , ""
  , ">>> f x"
  , "SomeConstructor [1,2,3]"
  , ">>> f x y z"
  , "SomeConstructor {"
  , "  x = 019343298472"
  , "  y = fdsifsiudfyoisudf"
  , "}"
  ]

sampleDoc3 = unlines $
  [ "The 'Eq' class defines equality ('==') and inequality ('/=')."
  , "All the basic datatypes exported by the \"Prelude\" are instances of 'Eq',"
  , "and 'Eq' may be derived for any datatype whose constituents are also"
  , "instances of 'Eq'."
  , ""
  , "The Haskell Report defines no laws for 'Eq'. However, '==' is customarily"
  , "expected to implement an equivalence relationship where two values comparing"
  , "equal are indistinguishable by \"public\" functions, with a \"public\" function"
  , "being one not allowing to see implementation details. For example, for a"
  , "type representing non-normalised natural numbers modulo 100, a \"public\""
  , "function doesn't make the difference between 1 and 201. It is expected to"
  , "have the following properties:"
  , ""
  , "[__Reflexivity__]: @x '==' x@ = 'True'"
  , "[__Symmetry__]: @x == y@ = @y == x@"
  , "[__Transitivity__]: if @x == y && y == z@ = 'True', then @x == z@ = 'True'"
  , "[__Substitutivity__]: if @x == y@ = 'True' and @f@ is a \"public\" function"
  , "whose return type is an instance of 'Eq', then @f x == f y@ = 'True'"
  , "[__Negation__]: @x /= y@ = @not (x == y)@"
  , ""
  , "Minimal complete definition: either '==' or '/='."
  ]
