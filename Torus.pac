| package |
package := Package name: 'Torus'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Punkt;
	add: #Spacer;
	add: #SpacerBlock;
	add: #SpacerConc;
	add: #SpacerLoop;
	add: #SpacerPunkt;
	add: #Torus;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #Punkt
	instanceVariableNames: 'torus index value'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Torus
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Collection subclass: #Spacer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Spacer subclass: #SpacerBlock
	instanceVariableNames: 'spacer metoda'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Spacer subclass: #SpacerConc
	instanceVariableNames: 'spacer1 spacer2'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Spacer subclass: #SpacerLoop
	instanceVariableNames: 'punkt kierunek wartosc'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Spacer subclass: #SpacerPunkt
	instanceVariableNames: 'punkt block'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Punkt guid: (GUID fromString: '{00CDF363-E24E-42E4-8E76-15ACF586DC04}')!
Punkt comment: ''!
!Punkt categoriesForClass!Kernel-Objects! !
!Punkt methodsFor!

- j
	(j == 0) ifTrue: [ ^self ]
	ifFalse: [
		| i new|
		i := index copy.
		new := (((index at: j) - 1) >= 0) ifTrue: [(index at: j) - 1] ifFalse: [(torus at: j) - 1].
		i at: j put: new.
		^Punkt init: i torus: torus value: value
	]!

% a
	^SpacerLoop punkt: self kierunek: (a value) wartosc: (a key)!

& block
	^SpacerPunkt punkt: self block: block!

@ vec
	| new |
	new := Array new: torus size.
	torus doWithIndex: [:el :in | new at: in put: ((index at: in) + (vec at: in) \\ (torus at: in))].
	^Punkt init: new torus: torus value: value!

| j
	^SpacerLoop punkt: self kierunek: j!

+ j
	(j == 0) ifTrue: [ ^self ]
	ifFalse: [
		| i new|
		i := index copy.
		new := ((index at: j) + 1) % (torus at: j).
		i at: j put: new.
		^Punkt init: i torus: torus value: value
	]!

init: ind torus: tor value: val
	index := ind.
	torus := tor.
	value := val.!

printOn: aStream
	index printOn: aStream.
	' ' printOn: aStream.
	(value at: index ifAbsent: [nil]) printOn: aStream.!

torus
	^torus!

value
	^value at: index ifAbsent: [nil]!

value: new 
	value at: index put: new! !
!Punkt categoriesFor: #-!public! !
!Punkt categoriesFor: #%!public! !
!Punkt categoriesFor: #&!public! !
!Punkt categoriesFor: #@!public! !
!Punkt categoriesFor: #|!public! !
!Punkt categoriesFor: #+!public! !
!Punkt categoriesFor: #init:torus:value:!public! !
!Punkt categoriesFor: #printOn:!public! !
!Punkt categoriesFor: #torus!public! !
!Punkt categoriesFor: #value!public! !
!Punkt categoriesFor: #value:!public! !

!Punkt class methodsFor!

init: ind torus: tor value: val
	^super new init: ind torus: tor value: val! !
!Punkt class categoriesFor: #init:torus:value:!public! !

Torus guid: (GUID fromString: '{21CD50E0-E816-4C83-98CB-E421809933BE}')!
Torus comment: ''!
!Torus categoriesForClass!Kernel-Objects! !
!Torus class methodsFor!

shape: s
	| p ind val|
	val := Dictionary new.
	ind := Array new: s size.
	ind atAllPut: 0.
	p := Punkt new.
	p init: ind torus: s value: val.
	^Punkt init: ind torus: s value: val! !
!Torus class categoriesFor: #shape:!public! !

Spacer guid: (GUID fromString: '{C654E848-930B-4A8D-9521-1995B7B3DC11}')!
Spacer comment: ''!
!Spacer categoriesForClass!Collections-Abstract! !
!Spacer methodsFor!

% j
	^SpacerBlock spacer: self metoda: [:p | p % j ]!

, t
	^SpacerConc spacer1: self spacer2: t!

| j
	^SpacerBlock spacer: self metoda: [:i | i | j ]!

first: anInteger
    | answer i |
    answer := OrderedCollection new.
    anInteger > 0 ifFalse: [^answer].
    i := anInteger.
    self do:
        [:each |
        answer add: each.
        i := i - 1.
        i = 0 ifTrue: [^answer]].
    ^answer!

printOn: aStream
"	'Spacer' printOn: aStream
"
	"Print a string representation of self on aStream. This method suffices for 
	most collections, and is able to handle cyclic references."

	| printed |
	printed := Processor activeProcess _alreadyPrinted.
	(printed includes: self) ifTrue: [^self printCyclicRefOn: aStream].
	printed add: self.
	
	[| tooMany |
	tooMany := aStream position + self maxPrint.
	self printPrefixOn: aStream.
	self do: 
			[:each | 
			aStream position > tooMany 
				ifTrue: 
					[aStream nextPutAll: '... etc ...'.
					^self].
			each printOn: aStream]
		separatedBy: [aStream space].
	self printSuffixOn: aStream] 
			ensure: [printed remove: self ifAbsent: []]
!

species
	^OrderedCollection! !
!Spacer categoriesFor: #%!public! !
!Spacer categoriesFor: #,!public! !
!Spacer categoriesFor: #|!public! !
!Spacer categoriesFor: #first:!public! !
!Spacer categoriesFor: #printOn:!public! !
!Spacer categoriesFor: #species!public! !

!Spacer class methodsFor!

new
	^self shouldNotImplement!

new: j
	^self shouldNotImplement! !
!Spacer class categoriesFor: #new!public! !
!Spacer class categoriesFor: #new:!public! !

SpacerBlock guid: (GUID fromString: '{42A2DFC2-78FC-47B3-9CC1-6FAD1BE21022}')!
SpacerBlock comment: ''!
!SpacerBlock categoriesForClass!Collections-Abstract! !
!SpacerBlock methodsFor!

do: operation
	| s |
	spacer do: [:p | 
		s := metoda value: p.
		s do: operation
	]!

spacer: sp metoda: md
	spacer := sp.
	metoda := md! !
!SpacerBlock categoriesFor: #do:!public! !
!SpacerBlock categoriesFor: #spacer:metoda:!public! !

!SpacerBlock class methodsFor!

spacer: sp metoda: md
	^super basicNew spacer: sp metoda: md
! !
!SpacerBlock class categoriesFor: #spacer:metoda:!public! !

SpacerConc guid: (GUID fromString: '{D80C37DD-2C7C-4B1B-A712-E550CFFE20A9}')!
SpacerConc comment: ''!
!SpacerConc categoriesForClass!Collections-Abstract! !
!SpacerConc methodsFor!

do: operation
	spacer1 do: operation.
	spacer2 do: operation!

spacer1: s spacer2: t
	spacer1 := s.
	spacer2 := t! !
!SpacerConc categoriesFor: #do:!public! !
!SpacerConc categoriesFor: #spacer1:spacer2:!public! !

!SpacerConc class methodsFor!

spacer1: s spacer2: t
	^super basicNew spacer1: s spacer2: t! !
!SpacerConc class categoriesFor: #spacer1:spacer2:!public! !

SpacerLoop guid: (GUID fromString: '{7A5DB919-AC2C-4DCF-99A5-A7B1523002DF}')!
SpacerLoop comment: ''!
!SpacerLoop categoriesForClass!Collections-Abstract! !
!SpacerLoop methodsFor!

do: operation
	(kierunek == 0) ifTrue: [ operation value: punkt ]
	ifFalse: [
		| p i op |
		wartosc > -1 ifTrue: [ i := wartosc ] ifFalse: [ i := punkt torus at: kierunek ].
		kierunek >= 0 ifTrue: [ op := [:a :b | a + b ] ] ifFalse: [ op := [:a :b | a - (b abs) ] ].
		p := punkt.
		1 to: i do: [:_ |
			operation value: p.
			p := op value: p value: kierunek.
		].
	]!

punkt: pkt kierunek: kier
	punkt := pkt.
	kierunek := kier.
	wartosc := -1!

punkt: pkt kierunek: kier wartosc: war
	punkt := pkt.
	kierunek := kier.
	wartosc := war! !
!SpacerLoop categoriesFor: #do:!public! !
!SpacerLoop categoriesFor: #punkt:kierunek:!public! !
!SpacerLoop categoriesFor: #punkt:kierunek:wartosc:!public! !

!SpacerLoop class methodsFor!

punkt: pkt kierunek: kier
	^super basicNew punkt: pkt kierunek: kier!

punkt: pkt kierunek: kier wartosc: war
	^super basicNew punkt: pkt kierunek: kier wartosc: war! !
!SpacerLoop class categoriesFor: #punkt:kierunek:!public! !
!SpacerLoop class categoriesFor: #punkt:kierunek:wartosc:!public! !

SpacerPunkt guid: (GUID fromString: '{310FC37A-0DF5-46F9-BF32-80DAEB9F084B}')!
SpacerPunkt comment: ''!
!SpacerPunkt categoriesForClass!Collections-Abstract! !
!SpacerPunkt methodsFor!

do: operation
	| s |
	operation value: punkt.
	s := block value: punkt.
	(s ~= nil) ifTrue: [ s do: operation ]!

punkt: pkt block: bl
	punkt := pkt.
	block := bl! !
!SpacerPunkt categoriesFor: #do:!public! !
!SpacerPunkt categoriesFor: #punkt:block:!public! !

!SpacerPunkt class methodsFor!

punkt: pkt block: bl
	^super basicNew punkt: pkt block: bl! !
!SpacerPunkt class categoriesFor: #punkt:block:!public! !

"Binary Globals"!

