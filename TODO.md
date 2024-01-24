# TODO

There are many functions from tidal that have not landed yet.

## Basics

- binding and join stuff: squeeze and trig

## Needs longer term work/thought

- fp or oop? or both? which syntax is better?
- parser hacks? metalua or hack the moonscript parser

## Low priority or won't implement

wedge
flatpat
<> operator (overlay/stack)
steps
stripe, slowstripe
step / step'
spread / spreadf / fastspread
append: not implemented, as made redundant by cat append is redundant #89
fastAppend: ditto with fastcat
slowAppend: ditto with slowcat
brak?

## Accumulation

 overlay => removed, as it's the same as stack see append is redundant #89

## Alteration

 rangex
 quantise
 ply
 stutter = echo
 palindrome = every(2, rev)
 trunc
 linger
 chunk, chunk'
 shuffle
 scramble
 rot
 spreadChoose / spreadr

## conditions

 every
 every' - what's this?
 whenmod - strange, but people do use it..
 sometimes, sometimesBy, someCycles, someCyclesBy
 choose, chooseby, wchoose, wchooseby - wchooseBy named wchooseWith in strudel
 struct
 mask
 sew
 stitch
 squeeze
 euclid, euclidLegato
 euclidInv, euclidFull
 ifp - what's this?

## Time

 hurry
 compress: is this compressSpan?
 zoom
 within
 off
 rotL / rotR - renamed 'early' and 'late' (in tidal these are usually used in their operator forms <~ and ~>
 rev
 jux
 juxBy
 swingBy / swing
 ghost
 inside / outside

## Harmony & Melody

Think these are just implemented differently in strudel?

scale
scaleList
getScale
toScale
chordList
arpeggiate
arp
Transitions

## these need block-based eval

anticipate / anticipateIn
clutch / clutchIn
histpan
interpolate / interpolateIn
jump / jumpIn / jumpIn' / jumpMod
wait / waitT
wash / washIn
xfade / xfadeIn

## Sampling

 chop
 striate
 striateBy
 loopAt
 segment
discretise - old name for segment.. lets just stick with the new one
 slice
 splice

## Randomness

 perlin / perlinWith
 perlin2 / perlin2With

## Composition

 ur
 seqP / seqPLoop
