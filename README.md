Right now just trying to get a wave to play with Netwire and
OpenAL. I'm trying to move from Yampa to Netwire since Yampa
is poorly documented, feels caught in an unfinished state,
and is not currently developed.

My current example plays smoothly under Yampa, but there are
popping sounds under Netwire in all my efforts. I think this
is because the signal isn't flowing continuously at a fast
enough rate. Perhaps I need a different manner of handing
the values from Netwire to OpenAL; different from this one
which worked with Yampa. I think Netwire itself should be
fast enough since it is designed for efficiency, although
maybe it wasn't intended for the rapid domain of audio
synthesis.

# Old stuff

plan
* immed
  * sin wave of arb freq
* dynamic music
  * possibly eventually for game integration
  * but hopefully still an external somewhat-general library?
* probabilistic
  * and "totally random" possibly even decent fallback, if all other
    computation not ready in time etc
* slider kinds of inputs?
  * what will these look like
    * mood
      * sad vs happy
    * simple vs complex?
    * tonal vs atonal?
    * consonant vs dissonant?
    * eerieness?
    * intensity?
    * general speed
    * meter
* grammar of tonal harmony progressions and transpositions?
* should try to build on what it has played
  * try to have reuse things to be interesting and musical
    * sequences
    * formats, more or less strict
      * can't be too strict to fit into probabilistic model
      * fugue?
  * try to use strategies from human improv technique?
    * nothing should seem like an accident
    * but improvisers
* make use of compositional techniques
  * again, sequences
* lots of things are hard
  * e.g. when are long silences ok
    (after cadences etc..)

sound libraries
* c
  * libsox
    * primarily: read and write different formats
    * apply various effects
    * play and record
  * portaudio
    * primarily: c callback -> play
    * play and record (raw audio only)
  * csound
    * simple music programming system
    * instruments _then_ notes
      * so not going to work with dynamic instruments
      * but maybe modifiable or lib bindings can already be more flex?
    * lots of contributed instrumentation
  * supercollider
    * more advanced programming system than csound
    * so probably just overhead if wrapping in hs frontend stuff?  idk..
* haskell
  * haskore
    * large (slow?) library for representing music
  * hamusic
    * more lightweight alternative to haskore?

