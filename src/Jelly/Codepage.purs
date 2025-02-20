module Jelly.Codepage
 ( Jel(..)
 , BuiltinForm(..)
 , unicode
 ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))

data BuiltinForm = Singlet Jel
                 | ChunkyNilad Jel
                 | ChunkyOMonad Jel
                 | ChunkyAMonad Jel
                 | ChunkyODyad Jel
                 | ChunkyADyad Jel


-- | ¡ 	¢ 	£ 	¤ 	¥ 	¦ 	© 	¬ 	® 	µ 	½ 	¿ 	€ 	Æ 	Ç 	Ð
-- | Ñ 	× 	Ø 	Œ 	Þ 	ß 	æ 	ç 	ð 	ı 	ȷ 	ñ 	÷ 	ø 	œ 	þ
-- |   	! 	" 	# 	$ 	% 	& 	' 	( 	) 	* 	+ 	, 	- 	. 	/
-- | 0 	1 	2 	3 	4 	5 	6 	7 	8 	9 	: 	; 	< 	= 	> 	?
-- | @ 	A 	B 	C 	D 	E 	F 	G 	H 	I 	J 	K 	L 	M 	N 	O
-- | P 	Q 	R 	S 	T 	U 	V 	W 	X 	Y 	Z 	[ 	\ 	] 	^ 	_
-- | ` 	a 	b 	c 	d 	e 	f 	g 	h 	i 	j 	k 	l 	m 	n 	o
-- | p 	q 	r 	s 	t 	u 	v 	w 	x 	y 	z 	{ 	| 	} 	~ 	¶
-- | ° 	¹ 	² 	³ 	⁴ 	⁵ 	⁶ 	⁷ 	⁸ 	⁹ 	⁺ 	⁻ 	⁼ 	⁽ 	⁾ 	Ɓ
-- | Ƈ 	Ɗ 	Ƒ 	Ɠ 	Ƙ 	Ɱ 	Ɲ 	Ƥ 	Ƭ 	Ʋ 	Ȥ 	ɓ 	ƈ 	ɗ 	ƒ 	ɠ
-- | ɦ 	ƙ 	ɱ 	ɲ 	ƥ 	ʠ 	ɼ 	ʂ 	ƭ 	ʋ 	ȥ 	Ạ 	Ḅ 	Ḍ 	Ẹ 	Ḥ
-- | Ị 	Ḳ 	Ḷ 	Ṃ 	Ṇ 	Ọ 	Ṛ 	Ṣ 	Ṭ 	Ụ 	Ṿ 	Ẉ 	Ỵ 	Ẓ 	Ȧ 	Ḃ
-- | Ċ 	Ḋ 	Ė 	Ḟ 	Ġ 	Ḣ 	İ 	Ŀ 	Ṁ 	Ṅ 	Ȯ 	Ṗ 	Ṙ 	Ṡ 	Ṫ 	Ẇ
-- | Ẋ 	Ẏ 	Ż 	ạ 	ḅ 	ḍ 	ẹ 	ḥ 	ị 	ḳ 	ḷ 	ṃ 	ṇ 	ọ 	ṛ 	ṣ
-- | ṭ 	§ 	Ä 	ẉ 	ỵ 	ẓ 	ȧ 	ḃ 	ċ 	ḋ 	ė 	ḟ 	ġ 	ḣ 	ŀ 	ṁ
-- | ṅ 	ȯ 	ṗ 	ṙ 	ṡ 	ṫ 	ẇ 	ẋ 	ẏ 	ż 	« 	» 	‘ 	’ 	“ 	”

data Jel = Lcxe -- yes this name is stupid BUT this character is important because it's zero so >:3
         | Cent
         | Pound
         | Currency
         | Yen
         | BrokenBar
         | Copyright
         | Not
         | Registered
         | Mu
         | Half
         | Euq
         | Euro
         | BigAsc
         | BigCCedilla
         | BigEth
         | BigEnye
         | Times
         | BigOSlash
         | BigOE
         | BigThorn
         | Eszett
         | LittleAsc
         | LittleCCedilla
         | LittleEth
         | LittleIDotless
         | LittleJDotless
         | LittleEnye
         | Obelus
         | LittleOSlash
         | LittleOE
         | LittleThorn
         | Space
         | Excl
         | DoubleQuote
         | Hash
         | Dollar
         | Percent
         | And
         | SingleQuote
         | OpenParen
         | CloseParen
         | Asterisk
         | Plus
         | Comma
         | HyphenMinus
         | Period
         | Slash
         | Zero
         | One
         | Two
         | Three
         | Four
         | Five
         | Six
         | Seven
         | Eight
         | Nine
         | Colon
         | Semicolon
         | Lt
         | Equals
         | Gt
         | Que
         | At
         | BigA
         | BigB
         | BigC
         | BigD -- heh. heh heh.
         | BigE
         | BigF
         | BigG
         | BigH
         | BigI
         | BigJ
         | BigK
         | BigL -- what I'm taking right now deciding to type this by hand for some fucking reason
         | BigM
         | BigN
         | BigO
         | BigP
         | BigQ
         | BigR
         | BigS
         | BigT
         | BigU
         | BigV
         | BigW
         | BigX
         | BigY
         | BigZ
         | OpenBracket
         | Backslash
         | CloseBracket
         | Caret
         | Underscore
         | Backtick
         | LittleA
         | LittleB
         | LittleC
         | LittleD
         | LittleE
         | LittleF
         | LittleG
         | LittleH
         | LittleI
         | LittleJ
         | LittleK
         | LittleL
         | LittleM
         | LittleN
         | LittleO
         | LittleP
         | LittleQ
         | LittleR
         | LittleS
         | LittleT
         | LittleU
         | LittleV
         | LittleW
         | LittleX
         | LittleY
         | LittleZ -- used Regenerate this time around LMAO
         | OpenBrace
         | Pipe
         | CloseBrace
         | Tilde
         | Newline
         | Degrees -- why is it just. there like it's a superscript 0 😭😭😭
         | SuperOne
         | SuperTwo
         | SuperThree
         | SuperFour
         | SuperFive
         | SuperSix
         | SuperSeven
         | SuperEight
         | SuperNine
         | SuperPlus
         | SuperMinus
         | SuperEquals
         | SuperOpenParen
         | SuperCloseParen
         | BigBHook
         | BigCHook
         | BigDHook
         | BigFHook
         | BigGHook
         | BigKHook
         | BigMHook
         | BigNHook
         | BigPHook
         | BigTHook
         | BigLabiodentalApproximant
         | BigZHook
         | LittleBHook
         | LittleCHook
         | LittleDHook
         | LittleFHook
         | LittleGHook
         | LittleHHook
         | LittleKHook
         | LittleMHook
         | LittleNHook
         | LittlePHook -- kinda looks like a fucked up thorn
         | LittleQHook
         | LittleRTail
         | LittleSHook
         | LittleTHook
         | LittleLabiodentalApproximant
         | LittleZHook
         | BigAUnderdot
         | BigBUnderdot
         | BigDUnderdot
         | BigEUnderdot
         | BigHUnderdot
         | BigIUnderdot
         | BigKUnderdot
         | BigLUnderdot
         | BigMUnderdot
         | BigNUnderdot
         | BigOUnderdot
         | BigRUnderdot
         | BigSUnderdot
         | BigTUnderdot
         | BigUUnderdot
         | BigVUnderdot
         | BigWUnderdot
         | BigYUnderdot
         | BigZUnderdot
         | BigAOverdot
         | BigBOverdot
         | BigCOverdot
         | BigDOverdot
         | BigEOverdot
         | BigFOverdot
         | BigGOverdot
         | BigHOverdot
         | BigIOverdot
         | BigLOverdot
         | BigMOverdot
         | BigNOverdot
         | BigOOverdot
         | BigPOverdot
         | BigROverdot
         | BigSOverdot
         | BigTOverdot
         | BigWOverdot
         | BigXOverdot
         | BigYOverdot
         | BigZOverdot
         | LittleAUnderdot
         | LittleBUnderdot
         | LittleDUnderdot
         | LittleEUnderdot
         | LittleFUnderdot
         | LittleIUnderdot
         | LittleKUnderdot
         | LittleLUnderdot
         | LittleMUnderdot
         | LittleNUnderdot
         | LittleOUnderdot
         | LittleRUnderdot -- thanks Ỵµt” Ḳ2ị),"ØJY for catching this TRAVESTY
         | LittleSUnderdot
         | LittleTUnderdot
         | Section
         | BigAUmlaut
         | LittleWUnderdot
         | LittleYUnderdot
         | LittleZUnderdot
         | LittleAOverdot
         | LittleBOverdot
         | LittleCOverdot
         | LittleDOverdot
         | LittleEOverdot
         | LittleFOverdot
         | LittleGOverdot
         | LittleHOverdot
         | LittleLOverdot
         | LittleMOverdot
         | LittleNOverdot
         | LittleOOverdot
         | LittlePOverdot
         | LittleROverdot
         | LittleSOverdot
         | LittleTOverdot
         | LittleWOverdot
         | LittleXOverdot
         | LittleYOverdot
         | LittleZOverdot
         | OpenGuillemet
         | CloseGuillemet
         | OpenSingleQuote
         | CloseSingleQuote
         | OpenDoubleQuote
         | CloseDoubleQuote

unicode :: Jel -> Char


unicode Jel = '¡'
unicode Cent = '¢'
unicode Pound = '£'
unicode Currency = '¤'
unicode Yen = '¥'
unicode BrokenBar = '¦'
unicode Copyright = '©'
unicode Not = '¬'
unicode Registered = '®'
unicode Mu = 'µ'
unicode Half = '½'
unicode Euq = '¿'
unicode Euro = '€'
unicode BigAsc = 'Æ'
unicode BigCCedilla = 'Ç'
unicode BigEth = 'Ð'
unicode BigEnye = 'Ñ'
unicode Times = '×'
unicode BigOSlash = 'Ø'
unicode BigOE = 'Œ'
unicode BigThorn = 'Þ'
unicode Eszett = 'ß'
unicode LittleAsc = 'æ'
unicode LittleCCedilla = 'ç'
unicode LittleEth = 'ð'
unicode LittleIDotless = 'ı'
unicode LittleJDotless = 'ȷ'
unicode LittleEnye = 'ñ'
unicode Obelus = '÷'
unicode LittleOSlash = 'ø'
unicode LittleOE = 'œ'
unicode LittleThorn = 'þ'
unicode Space = ' '
unicode Excl = '!'
unicode DoubleQuote = '"'
unicode Hash = '#'
unicode Dollar = '$'
unicode Percent = '%'
unicode And = '&'
unicode SingleQuote = '''
unicode OpenParen = '('
unicode CloseParen = ')'
unicode Asterisk = '*'
unicode Plus = '+'
unicode Comma = ','
unicode HyphenMinus = '-'
unicode Period = '.'
unicode Slash = '/'
unicode Zero = '0'
unicode One = '1'
unicode Two = '2'
unicode Three = '3'
unicode Four = '4'
unicode Five = '5'
unicode Six = '6'
unicode Seven = '7'
unicode Eight = '8'
unicode Nine = '9'
unicode Colon = ':'
unicode Semicolon = ';'
unicode Lt = '<'
unicode Equals = '='
unicode Gt = '>'
unicode Que = '?'
unicode At = '@'
unicode BigA = 'A'
unicode BigB = 'B'
unicode BigC = 'C'
unicode BigD = 'D'
unicode BigE = 'E'
unicode BigF = 'F'
unicode BigG = 'G'
unicode BigH = 'H'
unicode BigI = 'I'
unicode BigJ = 'J'
unicode BigK = 'K'
unicode BigL = 'L'
unicode BigM = 'M'
unicode BigN = 'N'
unicode BigO = 'O'
unicode BigP = 'P'
unicode BigQ = 'Q'
unicode BigR = 'R'
unicode BigS = 'S'
unicode BigT = 'T'
unicode BigU = 'U'
unicode BigV = 'V'
unicode BigW = 'W'
unicode BigX = 'X'
unicode BigY = 'Y'
unicode BigZ = 'Z'
unicode OpenBracket = '['
unicode Backslash = '\'
unicode CloseBracket = ']'
unicode Caret = '^'
unicode Underscore = '_'
unicode Backtick = '`'
unicode LittleA = 'a'
unicode LittleB = 'b'
unicode LittleC = 'c'
unicode LittleD = 'd'
unicode LittleE = 'e'
unicode LittleF = 'f'
unicode LittleG = 'g'
unicode LittleH = 'h'
unicode LittleI = 'i'
unicode LittleJ = 'j'
unicode LittleK = 'k'
unicode LittleL = 'l'
unicode LittleM = 'm'
unicode LittleN = 'n'
unicode LittleO = 'o'
unicode LittleP = 'p'
unicode LittleQ = 'q'
unicode LittleR = 'r'
unicode LittleS = 's'
unicode LittleT = 't'
unicode LittleU = 'u'
unicode LittleV = 'v'
unicode LittleW = 'w'
unicode LittleX = 'x'
unicode LittleY = 'y'
unicode LittleZ = 'z'
unicode OpenBrace = '{'
unicode Pipe = '|'
unicode CloseBrace = '}'
unicode Tilde = '~'
unicode Newline = '\n'
unicode Degrees = '°'
unicode SuperOne = '¹'
unicode SuperTwo = '²'
unicode SuperThree = '³'
unicode SuperFour = '⁴'
unicode SuperFive = '⁵'
unicode SuperSix = '⁶'
unicode SuperSeven = '⁷'
unicode SuperEight = '⁸'
unicode SuperNine = '⁹'
unicode SuperPlus = '⁺'
unicode SuperMinus = '⁻'
unicode SuperEquals = '⁼'
unicode SuperOpenParen = '⁽'
unicode SuperCloseParen = '⁾'
unicode BigBHook = 'Ɓ'
unicode BigCHook = 'Ƈ'
unicode BigDHook = 'Ɗ'
unicode BigFHook = 'Ƒ'
unicode BigGHook = 'Ɠ'
unicode BigKHook = 'Ƙ'
unicode BigMHook = 'Ɱ'
unicode BigNHook = 'Ɲ'
unicode BigPHook = 'Ƥ'
unicode BigTHook = 'Ƭ'
unicode BigLabiodentalApproximant = 'Ʋ'
unicode BigZHook = 'Ȥ'
unicode LittleBHook = 'ɓ'
unicode LittleCHook = 'ƈ'
unicode LittleDHook = 'ɗ'
unicode LittleFHook = 'ƒ'
unicode LittleGHook = 'ɠ'
unicode LittleHHook = 'ɦ'
unicode LittleKHook = 'ƙ'
unicode LittleMHook = 'ɱ'
unicode LittleNHook = 'ɲ'
unicode LittlePHook = 'ƥ'
unicode LittleQHook = 'ʠ'
unicode LittleRTail = 'ɼ'
unicode LittleSHook = 'ʂ'
unicode LittleTHook = 'ƭ'
unicode LittleLabiodentalApproximant = 'ʋ'
unicode LittleZHook = 'ȥ'
unicode BigAUnderdot = 'Ạ'
unicode BigBUnderdot = 'Ḅ'
unicode BigDUnderdot = 'Ḍ'
unicode BigEUnderdot = 'Ẹ'
unicode BigHUnderdot = 'Ḥ'
unicode BigIUnderdot = 'Ị'
unicode BigKUnderdot = 'Ḳ'
unicode BigLUnderdot = 'Ḷ'
unicode BigMUnderdot = 'Ṃ'
unicode BigNUnderdot = 'Ṇ'
unicode BigOUnderdot = 'Ọ'
unicode BigRUnderdot = 'Ṛ'
unicode BigSUnderdot = 'Ṣ'
unicode BigTUnderdot = 'Ṭ'
unicode BigUUnderdot = 'Ụ'
unicode BigVUnderdot = 'Ṿ'
unicode BigWUnderdot = 'Ẉ'
unicode BigYUnderdot = 'Ỵ'
unicode BigZUnderdot = 'Ẓ'
unicode BigAOverdot = 'Ȧ'
unicode BigBOverdot = 'Ḃ'
unicode BigCOverdot = 'Ċ'
unicode BigDOverdot = 'Ḋ'
unicode BigEOverdot = 'Ė'
unicode BigFOverdot = 'Ḟ'
unicode BigGOverdot = 'Ġ'
unicode BigHOverdot = 'Ḣ'
unicode BigIOverdot = 'İ'
unicode BigLOverdot = 'Ŀ'
unicode BigMOverdot = 'Ṁ'
unicode BigNOverdot = 'Ṅ'
unicode BigOOverdot = 'Ȯ'
unicode BigPOverdot = 'Ṗ'
unicode BigROverdot = 'Ṙ'
unicode BigSOverdot = 'Ṡ'
unicode BigTOverdot = 'Ṫ'
unicode BigWOverdot = 'Ẇ'
unicode BigXOverdot = 'Ẋ'
unicode BigYOverdot = 'Ẏ'
unicode BigZOverdot = 'Ż'
unicode LittleAUnderdot = 'ạ'
unicode LittleBUnderdot = 'ḅ'
unicode LittleDUnderdot = 'ḍ'
unicode LittleEUnderdot = 'ẹ'
unicode LittleFUnderdot = 'ḥ'
unicode LittleIUnderdot = 'ị'
unicode LittleKUnderdot = 'ḳ'
unicode LittleLUnderdot = 'ḷ'
unicode LittleMUnderdot = 'ṃ'
unicode LittleNUnderdot = 'ṇ'
unicode LittleOUnderdot = 'ọ'
unicode LittleRUnderdot = 'ṛ'
unicode LittleSUnderdot = 'ṣ'
unicode LittleTUnderdot = 'ṭ'
unicode Section = '§'
unicode BigAUmlaut = 'Ä'
unicode LittleWUnderdot = 'ẉ'
unicode LittleYUnderdot = 'ỵ'
unicode LittleZUnderdot = 'ẓ'
unicode LittleAOverdot = 'ȧ'
unicode LittleBOverdot = 'ḃ'
unicode LittleCOverdot = 'ċ'
unicode LittleDOverdot = 'ḋ'
unicode LittleEOverdot = 'ė'
unicode LittleFOverdot = 'ḟ'
unicode LittleGOverdot = 'ġ'
unicode LittleHOverdot = 'ḣ'
unicode LittleLOverdot = 'ŀ'
unicode LittleMOverdot = 'ṁ'
unicode LittleNOverdot = 'ṅ'
unicode LittleOOverdot = 'ȯ'
unicode LittlePOverdot = 'ṗ'
unicode LittleROverdot = 'ṙ'
unicode LittleSOverdot = 'ṡ'
unicode LittleTOverdot = 'ṫ'
unicode LittleWOverdot = 'ẇ'
unicode LittleXOverdot = 'ẋ'
unicode LittleYOverdot = 'ẏ'
unicode LittleZOverdot = 'ż'
unicode OpenGuillemet = '«'
unicode CloseGuillemet = '»'
unicode OpenSingleQuote = '‘'
unicode CloseSingleQuote = '’'
unicode OpenDoubleQuote = '“'
unicode CloseDoubleQuote = '”'

data WeirdWackyAlias = Pilcrow
                     | LittleUUnderdot -- so THAT'S why the lb bookmarklet is broken like that LMAO
                     | LittleVUnderdot

normalize :: WeirdWackyAlias -> Jel
normalize Pilcrow = Newline
normalize LittleUUnderdot = Section
normalize LittleVUnderdot = BigAUmlaut

builtinPrefix :: BuiltinForm -> Maybe Jel
builtinPrefix (Singlet _) = Nothing
builtinPrefix (ChunkyNilad _) = Just BigOSlash
builtinPrefix (ChunkyOMonad _) = Just BigOE
builtinPrefix (ChunkyAMonad _) = Just BigAsc
builtinPrefix (ChunkyODyad _) = Just LittleOE
builtinPrefix (ChunkyADyad _) = Just LittleAsc

builtinMainChar :: BuiltinForm -> Jel
builtinMainChar (Singlet c) = c 
builtinMainChar (ChunkyNilad c) = c
builtinMainChar (ChunkyOMonad c) = c
builtinMainChar (ChunkyAMonad c) = c
builtinMainChar (ChunkyODyad c) = c
builtinMainChar (ChunkyADyad c) = c

-- figure out what to actually do with this to get a nice Enum instance. Tomorrow. wow it's late
-- derive instance Generic Jel _ -- commented out for now because WHAT ARE THESE COMPILE TIMES ????
