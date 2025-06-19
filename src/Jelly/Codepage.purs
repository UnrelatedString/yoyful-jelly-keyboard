module Jelly.Codepage
  ( Jel(..)
  , BuiltinForm(..)
  , WeirdWackyAlias(..)
  , unicode
  , normalize
  , foldVariants
  , jelString
  ) where

import Prelude

import Data.Enum
  ( class Enum
  , class BoundedEnum
  , Cardinality(..)
  , defaultSucc
  , defaultPred
  , toEnum
  , fromEnum)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton)
import Data.Foldable (foldMap)

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

data Jel
  = Lcxe -- yes this name is stupid BUT this character is important because it's zero so >:3
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

unicode Lcxe = '¡'
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
unicode SingleQuote = '\''
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
unicode Backslash = '\\'
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
unicode Newline = '¶'
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

derive instance Eq Jel

instance Enum Jel where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance Ord Jel where
  compare = comparing fromEnum

instance Bounded Jel where
  top = CloseDoubleQuote
  bottom = Lcxe

instance BoundedEnum Jel where
  cardinality = Cardinality 256

  fromEnum Lcxe = 0
  fromEnum Cent = 1
  fromEnum Pound = 2
  fromEnum Currency = 3
  fromEnum Yen = 4
  fromEnum BrokenBar = 5
  fromEnum Copyright = 6
  fromEnum Not = 7
  fromEnum Registered = 8
  fromEnum Mu = 9
  fromEnum Half = 10
  fromEnum Euq = 11
  fromEnum Euro = 12
  fromEnum BigAsc = 13
  fromEnum BigCCedilla = 14
  fromEnum BigEth = 15
  fromEnum BigEnye = 16
  fromEnum Times = 17
  fromEnum BigOSlash = 18
  fromEnum BigOE = 19
  fromEnum BigThorn = 20
  fromEnum Eszett = 21
  fromEnum LittleAsc = 22
  fromEnum LittleCCedilla = 23
  fromEnum LittleEth = 24
  fromEnum LittleIDotless = 25
  fromEnum LittleJDotless = 26
  fromEnum LittleEnye = 27
  fromEnum Obelus = 28
  fromEnum LittleOSlash = 29
  fromEnum LittleOE = 30
  fromEnum LittleThorn = 31
  fromEnum Space = 32
  fromEnum Excl = 33
  fromEnum DoubleQuote = 34
  fromEnum Hash = 35
  fromEnum Dollar = 36
  fromEnum Percent = 37
  fromEnum And = 38
  fromEnum SingleQuote = 39
  fromEnum OpenParen = 40
  fromEnum CloseParen = 41
  fromEnum Asterisk = 42
  fromEnum Plus = 43
  fromEnum Comma = 44
  fromEnum HyphenMinus = 45
  fromEnum Period = 46
  fromEnum Slash = 47
  fromEnum Zero = 48
  fromEnum One = 49
  fromEnum Two = 50
  fromEnum Three = 51
  fromEnum Four = 52
  fromEnum Five = 53
  fromEnum Six = 54
  fromEnum Seven = 55
  fromEnum Eight = 56
  fromEnum Nine = 57
  fromEnum Colon = 58
  fromEnum Semicolon = 59
  fromEnum Lt = 60
  fromEnum Equals = 61
  fromEnum Gt = 62
  fromEnum Que = 63
  fromEnum At = 64
  fromEnum BigA = 65
  fromEnum BigB = 66
  fromEnum BigC = 67
  fromEnum BigD = 68
  fromEnum BigE = 69
  fromEnum BigF = 70
  fromEnum BigG = 71
  fromEnum BigH = 72
  fromEnum BigI = 73
  fromEnum BigJ = 74
  fromEnum BigK = 75
  fromEnum BigL = 76
  fromEnum BigM = 77
  fromEnum BigN = 78
  fromEnum BigO = 79
  fromEnum BigP = 80
  fromEnum BigQ = 81
  fromEnum BigR = 82
  fromEnum BigS = 83
  fromEnum BigT = 84
  fromEnum BigU = 85
  fromEnum BigV = 86
  fromEnum BigW = 87
  fromEnum BigX = 88
  fromEnum BigY = 89
  fromEnum BigZ = 90
  fromEnum OpenBracket = 91
  fromEnum Backslash = 92
  fromEnum CloseBracket = 93
  fromEnum Caret = 94
  fromEnum Underscore = 95
  fromEnum Backtick = 96
  fromEnum LittleA = 97
  fromEnum LittleB = 98
  fromEnum LittleC = 99
  fromEnum LittleD = 100
  fromEnum LittleE = 101
  fromEnum LittleF = 102
  fromEnum LittleG = 103
  fromEnum LittleH = 104
  fromEnum LittleI = 105
  fromEnum LittleJ = 106
  fromEnum LittleK = 107
  fromEnum LittleL = 108
  fromEnum LittleM = 109
  fromEnum LittleN = 110
  fromEnum LittleO = 111
  fromEnum LittleP = 112
  fromEnum LittleQ = 113
  fromEnum LittleR = 114
  fromEnum LittleS = 115
  fromEnum LittleT = 116
  fromEnum LittleU = 117
  fromEnum LittleV = 118
  fromEnum LittleW = 119
  fromEnum LittleX = 120
  fromEnum LittleY = 121
  fromEnum LittleZ = 122
  fromEnum OpenBrace = 123
  fromEnum Pipe = 124
  fromEnum CloseBrace = 125
  fromEnum Tilde = 126
  fromEnum Newline = 127
  fromEnum Degrees = 128
  fromEnum SuperOne = 129
  fromEnum SuperTwo = 130
  fromEnum SuperThree = 131
  fromEnum SuperFour = 132
  fromEnum SuperFive = 133
  fromEnum SuperSix = 134
  fromEnum SuperSeven = 135
  fromEnum SuperEight = 136
  fromEnum SuperNine = 137
  fromEnum SuperPlus = 138
  fromEnum SuperMinus = 139
  fromEnum SuperEquals = 140
  fromEnum SuperOpenParen = 141
  fromEnum SuperCloseParen = 142
  fromEnum BigBHook = 143
  fromEnum BigCHook = 144
  fromEnum BigDHook = 145
  fromEnum BigFHook = 146
  fromEnum BigGHook = 147
  fromEnum BigKHook = 148
  fromEnum BigMHook = 149
  fromEnum BigNHook = 150
  fromEnum BigPHook = 151
  fromEnum BigTHook = 152
  fromEnum BigLabiodentalApproximant = 153
  fromEnum BigZHook = 154
  fromEnum LittleBHook = 155
  fromEnum LittleCHook = 156
  fromEnum LittleDHook = 157
  fromEnum LittleFHook = 158
  fromEnum LittleGHook = 159
  fromEnum LittleHHook = 160
  fromEnum LittleKHook = 161
  fromEnum LittleMHook = 162
  fromEnum LittleNHook = 163
  fromEnum LittlePHook = 164
  fromEnum LittleQHook = 165
  fromEnum LittleRTail = 166
  fromEnum LittleSHook = 167
  fromEnum LittleTHook = 168
  fromEnum LittleLabiodentalApproximant = 169
  fromEnum LittleZHook = 170
  fromEnum BigAUnderdot = 171
  fromEnum BigBUnderdot = 172
  fromEnum BigDUnderdot = 173
  fromEnum BigEUnderdot = 174
  fromEnum BigHUnderdot = 175
  fromEnum BigIUnderdot = 176
  fromEnum BigKUnderdot = 177
  fromEnum BigLUnderdot = 178
  fromEnum BigMUnderdot = 179
  fromEnum BigNUnderdot = 180
  fromEnum BigOUnderdot = 181
  fromEnum BigRUnderdot = 182
  fromEnum BigSUnderdot = 183
  fromEnum BigTUnderdot = 184
  fromEnum BigUUnderdot = 185
  fromEnum BigVUnderdot = 186
  fromEnum BigWUnderdot = 187
  fromEnum BigYUnderdot = 188
  fromEnum BigZUnderdot = 189
  fromEnum BigAOverdot = 190
  fromEnum BigBOverdot = 191
  fromEnum BigCOverdot = 192
  fromEnum BigDOverdot = 193
  fromEnum BigEOverdot = 194
  fromEnum BigFOverdot = 195
  fromEnum BigGOverdot = 196
  fromEnum BigHOverdot = 197
  fromEnum BigIOverdot = 198
  fromEnum BigLOverdot = 199
  fromEnum BigMOverdot = 200
  fromEnum BigNOverdot = 201
  fromEnum BigOOverdot = 202
  fromEnum BigPOverdot = 203
  fromEnum BigROverdot = 204
  fromEnum BigSOverdot = 205
  fromEnum BigTOverdot = 206
  fromEnum BigWOverdot = 207
  fromEnum BigXOverdot = 208
  fromEnum BigYOverdot = 209
  fromEnum BigZOverdot = 210
  fromEnum LittleAUnderdot = 211
  fromEnum LittleBUnderdot = 212
  fromEnum LittleDUnderdot = 213
  fromEnum LittleEUnderdot = 214
  fromEnum LittleFUnderdot = 215
  fromEnum LittleIUnderdot = 216
  fromEnum LittleKUnderdot = 217
  fromEnum LittleLUnderdot = 218
  fromEnum LittleMUnderdot = 219
  fromEnum LittleNUnderdot = 220
  fromEnum LittleOUnderdot = 221
  fromEnum LittleRUnderdot = 222
  fromEnum LittleSUnderdot = 223
  fromEnum LittleTUnderdot = 224
  fromEnum Section = 225
  fromEnum BigAUmlaut = 226
  fromEnum LittleWUnderdot = 227
  fromEnum LittleYUnderdot = 228
  fromEnum LittleZUnderdot = 229
  fromEnum LittleAOverdot = 230
  fromEnum LittleBOverdot = 231
  fromEnum LittleCOverdot = 232
  fromEnum LittleDOverdot = 233
  fromEnum LittleEOverdot = 234
  fromEnum LittleFOverdot = 235
  fromEnum LittleGOverdot = 236
  fromEnum LittleHOverdot = 237
  fromEnum LittleLOverdot = 238
  fromEnum LittleMOverdot = 239
  fromEnum LittleNOverdot = 240
  fromEnum LittleOOverdot = 241
  fromEnum LittlePOverdot = 242
  fromEnum LittleROverdot = 243
  fromEnum LittleSOverdot = 244
  fromEnum LittleTOverdot = 245
  fromEnum LittleWOverdot = 246
  fromEnum LittleXOverdot = 247
  fromEnum LittleYOverdot = 248
  fromEnum LittleZOverdot = 249
  fromEnum OpenGuillemet = 250
  fromEnum CloseGuillemet = 251
  fromEnum OpenSingleQuote = 252
  fromEnum CloseSingleQuote = 253
  fromEnum OpenDoubleQuote = 254
  fromEnum CloseDoubleQuote = 255

  toEnum 0 = Just Lcxe
  toEnum 1 = Just Cent
  toEnum 2 = Just Pound
  toEnum 3 = Just Currency
  toEnum 4 = Just Yen
  toEnum 5 = Just BrokenBar
  toEnum 6 = Just Copyright
  toEnum 7 = Just Not
  toEnum 8 = Just Registered
  toEnum 9 = Just Mu
  toEnum 10 = Just Half
  toEnum 11 = Just Euq
  toEnum 12 = Just Euro
  toEnum 13 = Just BigAsc
  toEnum 14 = Just BigCCedilla
  toEnum 15 = Just BigEth
  toEnum 16 = Just BigEnye
  toEnum 17 = Just Times
  toEnum 18 = Just BigOSlash
  toEnum 19 = Just BigOE
  toEnum 20 = Just BigThorn
  toEnum 21 = Just Eszett
  toEnum 22 = Just LittleAsc
  toEnum 23 = Just LittleCCedilla
  toEnum 24 = Just LittleEth
  toEnum 25 = Just LittleIDotless
  toEnum 26 = Just LittleJDotless
  toEnum 27 = Just LittleEnye
  toEnum 28 = Just Obelus
  toEnum 29 = Just LittleOSlash
  toEnum 30 = Just LittleOE
  toEnum 31 = Just LittleThorn
  toEnum 32 = Just Space
  toEnum 33 = Just Excl
  toEnum 34 = Just DoubleQuote
  toEnum 35 = Just Hash
  toEnum 36 = Just Dollar
  toEnum 37 = Just Percent
  toEnum 38 = Just And
  toEnum 39 = Just SingleQuote
  toEnum 40 = Just OpenParen
  toEnum 41 = Just CloseParen
  toEnum 42 = Just Asterisk
  toEnum 43 = Just Plus
  toEnum 44 = Just Comma
  toEnum 45 = Just HyphenMinus
  toEnum 46 = Just Period
  toEnum 47 = Just Slash
  toEnum 48 = Just Zero
  toEnum 49 = Just One
  toEnum 50 = Just Two
  toEnum 51 = Just Three
  toEnum 52 = Just Four
  toEnum 53 = Just Five
  toEnum 54 = Just Six
  toEnum 55 = Just Seven
  toEnum 56 = Just Eight
  toEnum 57 = Just Nine
  toEnum 58 = Just Colon
  toEnum 59 = Just Semicolon
  toEnum 60 = Just Lt
  toEnum 61 = Just Equals
  toEnum 62 = Just Gt
  toEnum 63 = Just Que
  toEnum 64 = Just At
  toEnum 65 = Just BigA
  toEnum 66 = Just BigB
  toEnum 67 = Just BigC
  toEnum 68 = Just BigD
  toEnum 69 = Just BigE
  toEnum 70 = Just BigF
  toEnum 71 = Just BigG
  toEnum 72 = Just BigH
  toEnum 73 = Just BigI
  toEnum 74 = Just BigJ
  toEnum 75 = Just BigK
  toEnum 76 = Just BigL
  toEnum 77 = Just BigM
  toEnum 78 = Just BigN
  toEnum 79 = Just BigO
  toEnum 80 = Just BigP
  toEnum 81 = Just BigQ
  toEnum 82 = Just BigR
  toEnum 83 = Just BigS
  toEnum 84 = Just BigT
  toEnum 85 = Just BigU
  toEnum 86 = Just BigV
  toEnum 87 = Just BigW
  toEnum 88 = Just BigX
  toEnum 89 = Just BigY
  toEnum 90 = Just BigZ
  toEnum 91 = Just OpenBracket
  toEnum 92 = Just Backslash
  toEnum 93 = Just CloseBracket
  toEnum 94 = Just Caret
  toEnum 95 = Just Underscore
  toEnum 96 = Just Backtick
  toEnum 97 = Just LittleA
  toEnum 98 = Just LittleB
  toEnum 99 = Just LittleC
  toEnum 100 = Just LittleD
  toEnum 101 = Just LittleE
  toEnum 102 = Just LittleF
  toEnum 103 = Just LittleG
  toEnum 104 = Just LittleH
  toEnum 105 = Just LittleI
  toEnum 106 = Just LittleJ
  toEnum 107 = Just LittleK
  toEnum 108 = Just LittleL
  toEnum 109 = Just LittleM
  toEnum 110 = Just LittleN
  toEnum 111 = Just LittleO
  toEnum 112 = Just LittleP
  toEnum 113 = Just LittleQ
  toEnum 114 = Just LittleR
  toEnum 115 = Just LittleS
  toEnum 116 = Just LittleT
  toEnum 117 = Just LittleU
  toEnum 118 = Just LittleV
  toEnum 119 = Just LittleW
  toEnum 120 = Just LittleX
  toEnum 121 = Just LittleY
  toEnum 122 = Just LittleZ
  toEnum 123 = Just OpenBrace
  toEnum 124 = Just Pipe
  toEnum 125 = Just CloseBrace
  toEnum 126 = Just Tilde
  toEnum 127 = Just Newline
  toEnum 128 = Just Degrees
  toEnum 129 = Just SuperOne
  toEnum 130 = Just SuperTwo
  toEnum 131 = Just SuperThree
  toEnum 132 = Just SuperFour
  toEnum 133 = Just SuperFive
  toEnum 134 = Just SuperSix
  toEnum 135 = Just SuperSeven
  toEnum 136 = Just SuperEight
  toEnum 137 = Just SuperNine
  toEnum 138 = Just SuperPlus
  toEnum 139 = Just SuperMinus
  toEnum 140 = Just SuperEquals
  toEnum 141 = Just SuperOpenParen
  toEnum 142 = Just SuperCloseParen
  toEnum 143 = Just BigBHook
  toEnum 144 = Just BigCHook
  toEnum 145 = Just BigDHook
  toEnum 146 = Just BigFHook
  toEnum 147 = Just BigGHook
  toEnum 148 = Just BigKHook
  toEnum 149 = Just BigMHook
  toEnum 150 = Just BigNHook
  toEnum 151 = Just BigPHook
  toEnum 152 = Just BigTHook
  toEnum 153 = Just BigLabiodentalApproximant
  toEnum 154 = Just BigZHook
  toEnum 155 = Just LittleBHook
  toEnum 156 = Just LittleCHook
  toEnum 157 = Just LittleDHook
  toEnum 158 = Just LittleFHook
  toEnum 159 = Just LittleGHook
  toEnum 160 = Just LittleHHook
  toEnum 161 = Just LittleKHook
  toEnum 162 = Just LittleMHook
  toEnum 163 = Just LittleNHook
  toEnum 164 = Just LittlePHook
  toEnum 165 = Just LittleQHook
  toEnum 166 = Just LittleRTail
  toEnum 167 = Just LittleSHook
  toEnum 168 = Just LittleTHook
  toEnum 169 = Just LittleLabiodentalApproximant
  toEnum 170 = Just LittleZHook
  toEnum 171 = Just BigAUnderdot
  toEnum 172 = Just BigBUnderdot
  toEnum 173 = Just BigDUnderdot
  toEnum 174 = Just BigEUnderdot
  toEnum 175 = Just BigHUnderdot
  toEnum 176 = Just BigIUnderdot
  toEnum 177 = Just BigKUnderdot
  toEnum 178 = Just BigLUnderdot
  toEnum 179 = Just BigMUnderdot
  toEnum 180 = Just BigNUnderdot
  toEnum 181 = Just BigOUnderdot
  toEnum 182 = Just BigRUnderdot
  toEnum 183 = Just BigSUnderdot
  toEnum 184 = Just BigTUnderdot
  toEnum 185 = Just BigUUnderdot
  toEnum 186 = Just BigVUnderdot
  toEnum 187 = Just BigWUnderdot
  toEnum 188 = Just BigYUnderdot
  toEnum 189 = Just BigZUnderdot
  toEnum 190 = Just BigAOverdot
  toEnum 191 = Just BigBOverdot
  toEnum 192 = Just BigCOverdot
  toEnum 193 = Just BigDOverdot
  toEnum 194 = Just BigEOverdot
  toEnum 195 = Just BigFOverdot
  toEnum 196 = Just BigGOverdot
  toEnum 197 = Just BigHOverdot
  toEnum 198 = Just BigIOverdot
  toEnum 199 = Just BigLOverdot
  toEnum 200 = Just BigMOverdot
  toEnum 201 = Just BigNOverdot
  toEnum 202 = Just BigOOverdot
  toEnum 203 = Just BigPOverdot
  toEnum 204 = Just BigROverdot
  toEnum 205 = Just BigSOverdot
  toEnum 206 = Just BigTOverdot
  toEnum 207 = Just BigWOverdot
  toEnum 208 = Just BigXOverdot
  toEnum 209 = Just BigYOverdot
  toEnum 210 = Just BigZOverdot
  toEnum 211 = Just LittleAUnderdot
  toEnum 212 = Just LittleBUnderdot
  toEnum 213 = Just LittleDUnderdot
  toEnum 214 = Just LittleEUnderdot
  toEnum 215 = Just LittleFUnderdot
  toEnum 216 = Just LittleIUnderdot
  toEnum 217 = Just LittleKUnderdot
  toEnum 218 = Just LittleLUnderdot
  toEnum 219 = Just LittleMUnderdot
  toEnum 220 = Just LittleNUnderdot
  toEnum 221 = Just LittleOUnderdot
  toEnum 222 = Just LittleRUnderdot
  toEnum 223 = Just LittleSUnderdot
  toEnum 224 = Just LittleTUnderdot
  toEnum 225 = Just Section
  toEnum 226 = Just BigAUmlaut
  toEnum 227 = Just LittleWUnderdot
  toEnum 228 = Just LittleYUnderdot
  toEnum 229 = Just LittleZUnderdot
  toEnum 230 = Just LittleAOverdot
  toEnum 231 = Just LittleBOverdot
  toEnum 232 = Just LittleCOverdot
  toEnum 233 = Just LittleDOverdot
  toEnum 234 = Just LittleEOverdot
  toEnum 235 = Just LittleFOverdot
  toEnum 236 = Just LittleGOverdot
  toEnum 237 = Just LittleHOverdot
  toEnum 238 = Just LittleLOverdot
  toEnum 239 = Just LittleMOverdot
  toEnum 240 = Just LittleNOverdot
  toEnum 241 = Just LittleOOverdot
  toEnum 242 = Just LittlePOverdot
  toEnum 243 = Just LittleROverdot
  toEnum 244 = Just LittleSOverdot
  toEnum 245 = Just LittleTOverdot
  toEnum 246 = Just LittleWOverdot
  toEnum 247 = Just LittleXOverdot
  toEnum 248 = Just LittleYOverdot
  toEnum 249 = Just LittleZOverdot
  toEnum 250 = Just OpenGuillemet
  toEnum 251 = Just CloseGuillemet
  toEnum 252 = Just OpenSingleQuote
  toEnum 253 = Just CloseSingleQuote
  toEnum 254 = Just OpenDoubleQuote
  toEnum 255 = Just CloseDoubleQuote
  toEnum _ = Nothing

jelString :: Jel -> String
jelString = singleton <<< unicode

-- feels KINDA like an abuse of Show but like also. eh
instance Show Jel where
  show = singleton <<< unicode

data WeirdWackyAlias
  = Pilcrow
  | LittleUUnderdot -- so THAT'S why the lb bookmarklet is broken like that LMAO
  | LittleVUnderdot

normalize :: WeirdWackyAlias -> Jel
normalize Pilcrow = Newline
normalize LittleUUnderdot = Section
normalize LittleVUnderdot = BigAUmlaut

data BuiltinForm
  = Single Jel
  | O0 Jel
  | A1 Jel
  | A2 Jel
  | O1 Jel
  | O2 Jel
  | DQ Jel

builtinPrefix :: BuiltinForm -> Maybe Jel
builtinPrefix (Single _) = Nothing
builtinPrefix (O0 _) = Just BigOSlash
builtinPrefix (A1 _) = Just BigAsc
builtinPrefix (A2 _) = Just LittleAsc
builtinPrefix (O1 _) = Just BigOE
builtinPrefix (O2 _) = Just LittleOE
builtinPrefix (DQ _) = Just BigEth

builtinMainChar :: BuiltinForm -> Jel
builtinMainChar (Single c) = c
builtinMainChar (O0 c) = c
builtinMainChar (A1 c) = c
builtinMainChar (A2 c) = c
builtinMainChar (O1 c) = c
builtinMainChar (O2 c) = c
builtinMainChar (DQ c) = c

-- for collation in tooltips. am I even using "collation" right
foldVariants :: forall a. Semigroup a => (BuiltinForm -> a) -> Jel -> a
foldVariants = (>>>) O0 <> (>>>) A1 <> (>>>) A2 <> (>>>) O1 <> (>>>) O2

instance Show BuiltinForm where
  show b = foldMap show (builtinPrefix b) <> show (builtinMainChar b)
