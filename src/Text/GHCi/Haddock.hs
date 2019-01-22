{-# LANGUAGE OverloadedStrings #-}
module Text.GHCi.Haddock (
  -- * Pretty-printing
  prettyPrintHaddock, haddock2Doc,
  HaddockPrintConf(..),
  defaultHaddockConf,

  -- * Formatting options
  AnsiStyle,
  -- ** Color
  color, colorDull, bgColor, bgColorDull, Color(..),
  -- ** Style
  bold, italicized, underlined,
) where

-- base
import Control.Monad (join)
import Data.String   ( fromString )
import Data.Void     ( Void, absurd )
import Data.Char     ( isSpace )
import Data.List     ( dropWhileEnd )
import System.IO     ( Handle, stdout )

-- haddock-library
import Documentation.Haddock.Markup
import Documentation.Haddock.Parser
import Documentation.Haddock.Types

-- prettyprinter, prettyprinter-ansi-terminal
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

-- | Given a Haddock-formatted docstring, format and print that docstring to
-- the terminal.
prettyPrintHaddock :: String -> IO ()
prettyPrintHaddock = putDoc . haddock2Doc 

-- | Like 'prettyPrintDoc', but prints to an arbitrary 'Handle' insead of
-- 'stdout'.
haddock2Doc :: String -> Doc AnsiStyle
haddock2Doc doc = (blocksToDoc blks <> hardline)
  where
    MetaDoc { _doc = parsedDoc } = parseParas Nothing doc
    blks = getAsBlocks $ markup (terminalMarkup defaultHaddockConf) parsedDoc


-- | A Good Enough colour scheme
defaultHaddockConf :: HaddockPrintConf
defaultHaddockConf = HaddockPrintConf
  { hpc_default    = mempty
  , hpc_emphasis   = italicized
  , hpc_bold       = bold
  , hpc_monospaced = colorDull Magenta
  , hpc_header     = bold <> underlined <> color White
  , hpc_identifier = underlined <> color Magenta
  , hpc_math       = italicized <> color Green
  , hpc_links      = underlined <> color Blue
  , hpc_warning    = italicized <> color Red
  , hpc_control    = bold <> colorDull Yellow
  }

-- | Options for how to colour the terminal output
data HaddockPrintConf = HaddockPrintConf
  { hpc_default :: AnsiStyle
  -- ^ the default background

  , hpc_emphasis :: AnsiStyle
  -- ^ emphasized text

  , hpc_bold :: AnsiStyle
  -- ^ bold text

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

  , hpc_warning :: AnsiStyle
  -- ^ warning texts

  , hpc_control :: AnsiStyle
  -- ^ things like: equals in headers, bullets in lists, etc.
  }


type ReflowSpaces = Bool

-- | The main complexity with turning Haddock's 'DocH' into a
-- @'Doc' 'AnsiStyle'@ is that there is often a conflation of
-- inline and block-level elements.
--
-- We cheat by choosing a worker which tries both at once.
data RenderedDocH = RDH
  { getAsBlocks :: [Doc AnsiStyle]               -- ^ render as blocks
  , getAsInline :: ReflowSpaces -> Doc AnsiStyle -- ^ render as inline
  }

-- | Concatenate a bunch of blocks vertically with empty lines between blocks
blocksToDoc :: [Doc AnsiStyle] -> Doc AnsiStyle
blocksToDoc = align . vcat . punctuate hardline

-- | Markup for turning a 'DocH' into a 'RenderedDocH'
terminalMarkup :: HaddockPrintConf -> DocMarkupH Void Identifier RenderedDocH
terminalMarkup hpc = Markup
  { markupEmpty = RDH { getAsBlocks = []
                      , getAsInline = mempty }

  -- This is where reflow spaces matters: we only split the string into words
  -- and glue those words back together if we have the go-ahead to reflow.
  , markupString = \str -> onlyInline $ \reflowSpaces -> case str of
      "" -> mempty
      _  | reflowSpaces
         -> let headSpace = if isSpace (head str) then space else mempty
                lastSpace = if isSpace (last str) then space else mempty
             in headSpace <> fillSep (map fromString (words str)) <> lastSpace
         | otherwise
         -> fromString (dropWhileEnd (== '\n') str)

  , markupParagraph = \doc -> onlyBlock (getAsInline doc True)

  , markupAppend = \(RDH b1 i1) (RDH b2 i2) -> RDH (b1 ++ b2) (i1 <> i2)

  , markupIdentifier = \(_,idnt,_) -> onlyInline $ \_ -> ident (fromString idnt)
  , markupModule     = \mdl        -> onlyInline $ \_ -> ident (fromString mdl)
  , markupAName      = \anc        -> onlyInline $ \_ -> ident (fromString anc)

  , markupIdentifierUnchecked = absurd

  , markupEmphasis   = \doc        -> onlyInline (emph   . getAsInline doc)
  , markupBold       = \doc        -> onlyInline (bolded . getAsInline doc)
  , markupMonospaced = \doc        -> onlyInline (mono   . getAsInline doc)
    
  , markupWarning = \doc ->
      onlyBlock (warn (getAsInline doc True))

  , markupUnorderedList = \docs ->
      onlyBlock (renderListLike (repeat "*")
                                (map (blocksToDoc . getAsBlocks) docs))

  , markupOrderedList = \docs ->
      onlyBlock (renderListLike [ unsafeViaShow i <> "." | i <- [1 :: Int ..] ]
                                (map (blocksToDoc . getAsBlocks) docs))

  , markupDefList = \lblDocs -> let (lbls, docs) = unzip lblDocs in
      onlyBlock (renderListLike [ ctrl (getAsInline l True <> ":") <> hardline | l <- lbls ]
                                (map (\doc -> align (getAsInline doc False)) docs))

  -- Render markdown-style only when we have a title
  , markupHyperlink = \(Hyperlink uri titleOpt) -> onlyInline $ \_ -> case titleOpt of
      Nothing    -> ctrl "<" <> link (fromString uri) <> ctrl ">"
      Just title -> ctrl "[" <> fromString title <> ctrl "](" <> link (fromString uri) <> ctrl ")"

  -- Render markdown-style only when we have a title
  , markupPic = \(Picture uri titleOpt) -> onlyInline $ \_ -> case titleOpt of
      Nothing    -> ctrl "<<" <> link (fromString uri) <> ctrl ">>"
      Just title -> ctrl "![" <> fromString title <> ctrl "](" <> link (fromString uri) <> ctrl ")"

  , markupMathInline = \tex -> onlyInline $ \_ ->
      math ("\\(" <+> fillSep (map fromString (words tex)) <+> "\\)")

  , markupMathDisplay = \tex -> onlyBlock (math ("\\[" <> fromString tex <> "\\]"))

  , markupProperty = \prop -> onlyBlock (ctrl "prop>" <> fromString prop)

  , markupHeader = \(Header lvl title) -> let leader = ctrl (fromString (replicate lvl '=')) in
      onlyBlock (leader <+> header (getAsInline title True))

  -- TODO: figure out a good way to render this
  , markupTable = \_ -> onlyBlock (bad "<table could not be rendered>")

  , markupExample = \examples -> onlyBlock . vcat . join $
      [ (ctrl ">>>" <+> fromString input) : (map (mono . fromString) output)
      | Example input output <- examples ]

  -- This is where we ask for an inline block with spaces /not/ reflowed
  , markupCodeBlock = \doc -> RDH { getAsBlocks = [mono (getAsInline doc False)]
                                  , getAsInline = \_ -> mono (getAsInline doc True) }
  }
  where
    -- This element is really and inline one, so interpretting it as a block
    -- is a best effort.
    onlyInline :: (ReflowSpaces -> Doc AnsiStyle) -> RenderedDocH
    onlyInline renderInline = RDH { getAsBlocks = [renderInline False]
                                  , getAsInline = renderInline }

    -- This element is really a block one, so interpretting it as inline is a
    -- best effort.
    onlyBlock :: Doc AnsiStyle -> RenderedDocH
    onlyBlock renderBlock   = RDH { getAsBlocks = [renderBlock]
                                  , getAsInline = \_ -> align renderBlock }

    -- Given what the bullets look like and elements associated with the
    -- bullets, produce the doc.
    renderListLike :: [Doc AnsiStyle] -> [Doc AnsiStyle] -> Doc AnsiStyle
    renderListLike ixs docs = indent 2 . blocksToDoc $
      [ ctrl ix <+> doc | (ix,doc) <- ixs `zip` docs ]

    -- Useful annotations
    header = annotate (hpc_header     hpc)
    emph   = annotate (hpc_emphasis   hpc)
    bolded = annotate (hpc_bold       hpc)
    ctrl   = annotate (hpc_control    hpc)
    link   = annotate (hpc_links      hpc)
    math   = annotate (hpc_math       hpc)
    mono   = annotate (hpc_monospaced hpc)
    ident  = annotate (hpc_identifier hpc)
    warn   = annotate (hpc_warning    hpc)
    bad    = annotate (color Red <> bold <> bgColor White)

