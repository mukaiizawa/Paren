; wordle.

(import :console)
(import :rand)

(<- $dict
    '(april abbey abhor abide abyss acorn actor adept affix again agile agree ahead aisle
      alarm alias alibi align alike alive allay allow alloy aloft aloud alter amass amber
      amend anger angry anvil apart arson aside aspen atlas awake aware basic basis bathe
      beach begin below berth beset binge birth black blank bless blind bloke board boast
      booze bored bosom bound brain brake brash brawn bread break brine bring broad brook
      brunt brush bully carry catch cause chair chant chart chase chasm cheap cheek cheer
      chest chief child choke claim clean clear clerk cliff climb close cloth cloud color
      cough count court crack crash crazy crime crock crone crony cross crowd crown cruel
      crush curio cycle daily daunt death debit decay decoy defer deity delay delve depth
      devil dirty doing doubt dozen dream drink drive drown drunk eager early earth edict
      elude empty enact enemy enjoy enter equal erode error ethos event every exact exist
      faith famed fatty favor fever fiend fight filth first fixed flare flick flirt flock
      flora flour fluke foray force found frank fresh front fussy gamut glare goods goody
      goose gourd graft grass great greet gripe grope grown guess gusto habit happy heave
      heavy hinge hoard hoist horse house human hunch hurry inert inlet inner jewel jolly
      lance lapse large larva later learn least leave leech lefty legal level local loose
      lousy lunch marry match maybe merry minor money month mount mourn mouth music naked
      nasty nerve never niece night noble noise nudge occur ocean offer often opium other
      oxide patch peace petal piece pilot pique plain plant plate plumb point pound power
      prawn press prick prize proof proud prove pupil quail quell quick quiet quite rainy
      raise rapid reach ready refer relax reply rhyme right rigor rival river rough round
      royal ruler rumor rupee sadly scale scare scene scope screw sense serve shade shake
      shame shape share sharp shear sheer shelf shell shine shore short shout sight skill
      slack slave sleep slide small smear smile solid solve sorry space speak spend spice
      spill split spoil spook sport spurt stair stand start state steal steam steel stiff
      still sting store storm straw stuff stump sunny swear sweat sweep swell swing table
      tacit tacky taste taunt thank thick thief thing think third throb throw thumb tight
      tired token tooth total tough track trade train tramp trash treat trend trial truly
      trunk trust truth twice twist ulcer uncle union upper upset urban usher usual valid
      value visit vital vivid voice wages waste watch water weigh wheel whole whore widen
      width world worry worse worst worth wound wrath wreck write wrong young youth)
    $max-times 6)

(function print-hyphen ()
  (dotimes (i 11) (console.write "-"))
  (console.write "\n"))

(function refresh ()
  (console.clear)
  (print-hyphen)
  (dotimes (i $max-times)
    (let (word ([] (reverse $histories) i))
      (console.write "|")
      (dotimes (j 5)
        (let (ch ([] word j))
          (if (= ch ([] $word j)) (console.write ch :color :black :bg-color :green)
              (in? ch $word) (console.write ch :color :black :bg-color :yellow)
              (empty? ch) (console.write " " :color :white :bg-color :black)
              (console.write ch :color :white :bg-color :black)))
        (console.write "|"))
      (console.write "\n"))
      (print-hyphen))
  (console.write ") "))

(function input ()
  (let (line (lower (read-line)))
    (if (|| (! (alpha? line)) (! (in? (symbol line) $dict)))
        (begin
          (println "again")
          (input))
        (push! (split line) $histories))))

(function! main (args)
  (rand.seed (time))
  (<- $word (split (str (rand.choice $dict)))
      $histories nil)
  (let (word nil)
    (while (<= (len $histories) $max-times)
      (refresh)
      (if (= (<- word (input)) $word) (break)))
    (print-hyphen)
    (println (join $word))
    (print-hyphen)
    (if (!= word $word) (println "lose")
        (println "win"))))
