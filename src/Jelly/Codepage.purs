module Jelly.Codepage
 ( Jel(..)
 , BuiltinForm(..)
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

data Jel = Lcxe
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
         | BigZeHautsia
         | BigEth
         | BigEnye
         | Times
         | BigOSlash
         | BigOE
         | BigThorn
         | Eszett
         | LittleAsc
         | LittleZeHautsia
         | LittleEth
         | LittleTurkishI
         | LittleKarelianJ
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
         | BigO -- if only
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



data WeirdWackyAlias = Pilcrow
                     | LittleUUnderdot -- so THAT'S why the lb bookmarklet is broken like that LMAO
                     | LittleVUnderdot

normalize :: WeirdWackyAlias -> Jel
normalize Pilcrow = Newline
normalize LittleUUnderdot = Section
normalize LittleVUnderdot = BigAUmlaut

prefix :: BuiltinForm -> Maybe Jel
prefix Singlet = Nothing
prefix ChunkyNilad _ = Just BigOSlash
prefix ChunkyOMonad _ = Just BigOE
prefix ChunkyAMonad _ = Just BigAsc
prefix ChunkyODyad _ = Just LittleOE
prefix ChunkyADyad _ = Just LittleAsc

-- figure out what to actually do with this to get a nice Enum instance. Tomorrow. wow it's late
derive instance Generic Jel
