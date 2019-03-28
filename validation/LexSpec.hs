module LexSpec where

import Data.Char (isControl, ord)
import Numeric (showHex)
import Text.Parsec (parse) 
import Test.Hspec (describe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck as QuickCheck

import Keywords (keywords)
import Lexer


spec =
  modifyMaxSuccess (const 10000) $
    describe "tokens" $
      prop "preserves tokens that have be encoded as a string" $ \ts ->
        parse tokens "" (untokenize ts) === Right ts



-- ARBITRARY


instance Arbitrary Token where
  arbitrary =
      frequency
          [ (9, Keyword <$> elements keywords)
          , (5, UIntLit <$> QuickCheck.arbitrarySizedNatural)
          , (5, SIntLit <$> QuickCheck.arbitrarySizedIntegral)
          , (5, StringLit <$> arbitraryStringLit)
          , (5, Id <$> arbitraryId)
          , (1, return OpenParen)
          , (1, return CloseParen)
          , (3, Reserved <$> QuickCheck.sublistOf idChar)
          ]



arbitraryId :: Gen String
arbitraryId = do
    idchars <- QuickCheck.sublistOf idChar
    return ('$' : idchars)


arbitraryStringLit :: Gen String
arbitraryStringLit = do
    str <- QuickCheck.listOf arbitraryStringElem
    return $ concat str


arbitraryStringElem :: Gen String
arbitraryStringElem =
    QuickCheck.frequency
        [ (9, arbitraryStringChars)
        , (1, arbitraryRawByte)
        ]


arbitraryStringChars :: Gen String
arbitraryStringChars = do
    cs <- QuickCheck.frequency
             [ (9, arbitraryChar)
             , (1, arbitraryEscapeSequence)
             ]
    return cs


arbitraryChar :: Gen String
arbitraryChar = do
    c <- QuickCheck.suchThat QuickCheck.arbitraryUnicodeChar 
             (\d -> d /= '\\' && d /= '\"' && not (isControl d))
    return [c]


arbitraryEscapeSequence :: Gen String
arbitraryEscapeSequence = do
    cs <- QuickCheck.frequency
             [ (9, arbitraryEscapeChar)
             , (3, arbitraryUnicodeEncoding)
             ]
    return ('\\' : cs)


arbitraryEscapeChar :: Gen String
arbitraryEscapeChar = do
    c <- QuickCheck.elements "tnr\"'\\"
    return [c]


arbitraryUnicodeEncoding :: Gen String
arbitraryUnicodeEncoding = do
    c <- QuickCheck.arbitraryUnicodeChar
    return ("u{" ++ showHex (ord c) "" ++ "}")
    

arbitraryRawByte :: Gen String
arbitraryRawByte = do
    n <- QuickCheck.elements hexChars 
    m <- QuickCheck.elements hexChars
    return ('\\' : [n,m])


hexChars :: String
hexChars =
    ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']


  
-- UNTOKENIZE 
  

untoken :: Token -> String 
untoken token =
    case token of
        Keyword kw    -> kw
        UIntLit n     -> show n
        SIntLit n     -> if n >= 0 then "+" ++ show n else show n
        StringLit str -> "\"" ++ str ++ "\""
        Id ident      -> ident
        OpenParen     -> "("
        CloseParen    -> ")"
        Reserved r    -> r


untokenize :: [Token] -> String
untokenize =
    unwords . map untoken
