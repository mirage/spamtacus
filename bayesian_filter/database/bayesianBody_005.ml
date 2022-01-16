open Database
open BayesianBody_004
let db = db 
|> Map.add "thank" { in_spam=53; in_ham=38}
|> Map.add "thanks" { in_spam=18; in_ham=262}
|> Map.add "that" { in_spam=241; in_ham=1562}
|> Map.add "that's" { in_spam=39; in_ham=284}
|> Map.add "thats" { in_spam=0; in_ham=26}
|> Map.add "the" { in_spam=440; in_ham=2206}
|> Map.add "theadmanager" { in_spam=15; in_ham=0}
|> Map.add "theater" { in_spam=1; in_ham=11}
|> Map.add "theft" { in_spam=1; in_ham=14}
|> Map.add "their" { in_spam=97; in_ham=436}
|> Map.add "them" { in_spam=90; in_ham=408}
|> Map.add "themselves" { in_spam=10; in_ham=36}
|> Map.add "then" { in_spam=79; in_ham=467}
|> Map.add "theo" { in_spam=1; in_ham=16}
|> Map.add "theory" { in_spam=0; in_ham=38}
|> Map.add "therapy" { in_spam=9; in_ham=3}
|> Map.add "there" { in_spam=76; in_ham=713}
|> Map.add "there's" { in_spam=17; in_ham=184}
|> Map.add "therefore" { in_spam=19; in_ham=25}
|> Map.add "thereof" { in_spam=1; in_ham=9}
|> Map.add "these" { in_spam=91; in_ham=280}
|> Map.add "they" { in_spam=66; in_ham=657}
|> Map.add "they'd" { in_spam=3; in_ham=20}
|> Map.add "they'll" { in_spam=0; in_ham=23}
|> Map.add "they're" { in_spam=10; in_ham=77}
|> Map.add "they've" { in_spam=1; in_ham=28}
|> Map.add "thimm" { in_spam=0; in_ham=9}
|> Map.add "thing" { in_spam=24; in_ham=203}
|> Map.add "things" { in_spam=19; in_ham=222}
|> Map.add "think" { in_spam=35; in_ham=422}
|> Map.add "thinkgeek" { in_spam=2; in_ham=101}
|> Map.add "thinking" { in_spam=14; in_ham=62}
|> Map.add "thinks" { in_spam=0; in_ham=34}
|> Map.add "third" { in_spam=16; in_ham=51}
|> Map.add "thirty" { in_spam=9; in_ham=7}
|> Map.add "this" { in_spam=381; in_ham=1431}
|> Map.add "tho" { in_spam=0; in_ham=9}
|> Map.add "thomas" { in_spam=2; in_ham=27}
|> Map.add "those" { in_spam=52; in_ham=262}
|> Map.add "though" { in_spam=17; in_ham=198}
|> Map.add "thought" { in_spam=11; in_ham=139}
|> Map.add "thoughts" { in_spam=0; in_ham=19}
|> Map.add "thousand" { in_spam=18; in_ham=20}
|> Map.add "thousands" { in_spam=48; in_ham=37}
|> Map.add "thread" { in_spam=1; in_ham=22}
|> Map.add "threads" { in_spam=0; in_ham=14}
|> Map.add "threat" { in_spam=3; in_ham=15}
|> Map.add "threaten" { in_spam=0; in_ham=12}
|> Map.add "threatening" { in_spam=3; in_ham=10}
|> Map.add "threats" { in_spam=5; in_ham=12}
|> Map.add "three" { in_spam=26; in_ham=109}
|> Map.add "threshold" { in_spam=0; in_ham=8}
|> Map.add "through" { in_spam=83; in_ham=245}
|> Map.add "throughout" { in_spam=5; in_ham=24}
|> Map.add "throw" { in_spam=3; in_ham=19}
|> Map.add "throwing" { in_spam=3; in_ham=11}
|> Map.add "thru" { in_spam=2; in_ham=15}
|> Map.add "thu" { in_spam=3; in_ham=133}
|> Map.add "thursday" { in_spam=0; in_ham=60}
|> Map.add "thus" { in_spam=8; in_ham=45}
|> Map.add "tickets" { in_spam=3; in_ham=8}
|> Map.add "tid" { in_spam=0; in_ham=9}
|> Map.add "tie" { in_spam=0; in_ham=9}
|> Map.add "tied" { in_spam=0; in_ham=17}
|> Map.add "ties" { in_spam=2; in_ham=7}
|> Map.add "till" { in_spam=4; in_ham=10}
|> Map.add "tim" { in_spam=1; in_ham=55}
|> Map.add "tim's" { in_spam=0; in_ham=10}
|> Map.add "time" { in_spam=158; in_ham=540}
|> Map.add "timely" { in_spam=4; in_ham=9}
|> Map.add "timeout" { in_spam=0; in_ham=9}
|> Map.add "times" { in_spam=55; in_ham=151}
|> Map.add "timtest" { in_spam=0; in_ham=13}
|> Map.add "tiny" { in_spam=1; in_ham=18}
|> Map.add "tip" { in_spam=6; in_ham=16}
|> Map.add "tips" { in_spam=7; in_ham=14}
|> Map.add "tired" { in_spam=9; in_ham=116}
|> Map.add "title" { in_spam=154; in_ham=25}
|> Map.add "titles" { in_spam=4; in_ham=8}
|> Map.add "tmda" { in_spam=0; in_ham=20}
|> Map.add "tmp" { in_spam=0; in_ham=34}
|> Map.add "tmpl" { in_spam=0; in_ham=9}
|> Map.add "tobacco" { in_spam=12; in_ham=2}
|> Map.add "today" { in_spam=130; in_ham=142}
|> Map.add "today's" { in_spam=9; in_ham=30}
|> Map.add "together" { in_spam=10; in_ham=69}
|> Map.add "toilet" { in_spam=1; in_ham=7}
|> Map.add "tokenize" { in_spam=0; in_ham=19}
|> Map.add "tokenizer" { in_spam=0; in_ham=26}
|> Map.add "tokens" { in_spam=0; in_ham=9}
|> Map.add "told" { in_spam=14; in_ham=79}
|> Map.add "toll-free" { in_spam=22; in_ham=1}
|> Map.add "tom" { in_spam=0; in_ham=73}
|> Map.add "tomorrow" { in_spam=3; in_ham=25}
|> Map.add "ton" { in_spam=1; in_ham=7}
|> Map.add "tone" { in_spam=3; in_ham=7}
|> Map.add "tonight" { in_spam=0; in_ham=15}
|> Map.add "tons" { in_spam=1; in_ham=13}
|> Map.add "tony" { in_spam=0; in_ham=49}
|> Map.add "too" { in_spam=37; in_ham=281}
|> Map.add "took" { in_spam=21; in_ham=59}
|> Map.add "tool" { in_spam=13; in_ham=42}
|> Map.add "tools" { in_spam=23; in_ham=53}
|> Map.add "top" { in_spam=109; in_ham=89}
|> Map.add "topic" { in_spam=0; in_ham=16}
|> Map.add "topics" { in_spam=3; in_ham=7}
|> Map.add "topmargin" { in_spam=22; in_ham=0}
|> Map.add "total" { in_spam=35; in_ham=51}
|> Map.add "totally" { in_spam=17; in_ham=30}
|> Map.add "touch" { in_spam=3; in_ham=24}
|> Map.add "tough" { in_spam=5; in_ham=5}
|> Map.add "toward" { in_spam=2; in_ham=16}
|> Map.add "towards" { in_spam=5; in_ham=33}
|> Map.add "towers" { in_spam=0; in_ham=9}
|> Map.add "town" { in_spam=6; in_ham=21}
|> Map.add "toys" { in_spam=0; in_ham=10}
|> Map.add "trace" { in_spam=4; in_ham=8}
|> Map.add "traceback" { in_spam=0; in_ham=14}
|> Map.add "track" { in_spam=26; in_ham=34}
|> Map.add "tracking" { in_spam=1; in_ham=15}
|> Map.add "trade" { in_spam=20; in_ham=69}
|> Map.add "trademark" { in_spam=0; in_ham=11}
|> Map.add "trading" { in_spam=10; in_ham=5}
|> Map.add "tradition" { in_spam=0; in_ham=9}
|> Map.add "traditional" { in_spam=7; in_ham=21}
|> Map.add "traffic" { in_spam=7; in_ham=51}
|> Map.add "trails" { in_spam=0; in_ham=8}
|> Map.add "train" { in_spam=11; in_ham=25}
|> Map.add "trained" { in_spam=0; in_ham=16}
|> Map.add "training" { in_spam=17; in_ham=52}
|> Map.add "transaction" { in_spam=32; in_ham=15}
|> Map.add "transactions" { in_spam=0; in_ham=15}
|> Map.add "transfer" { in_spam=34; in_ham=19}
|> Map.add "transferred" { in_spam=9; in_ham=6}
|> Map.add "transferring" { in_spam=5; in_ham=5}
|> Map.add "transforming" { in_spam=0; in_ham=9}
|> Map.add "transitional" { in_spam=29; in_ham=8}
|> Map.add "translate" { in_spam=0; in_ham=9}
|> Map.add "transmitted" { in_spam=1; in_ham=7}
|> Map.add "travel" { in_spam=11; in_ham=10}
|> Map.add "treated" { in_spam=5; in_ham=12}
|> Map.add "treatment" { in_spam=0; in_ham=14}
|> Map.add "tree" { in_spam=0; in_ham=27}
|> Map.add "trees" { in_spam=4; in_ham=11}
|> Map.add "trek" { in_spam=0; in_ham=8}
|> Map.add "tremendous" { in_spam=13; in_ham=4}
|> Map.add "trend" { in_spam=2; in_ham=10}
|> Map.add "trial" { in_spam=8; in_ham=7}
|> Map.add "trick" { in_spam=4; in_ham=31}
|> Map.add "tricks" { in_spam=3; in_ham=10}
|> Map.add "tricky" { in_spam=0; in_ham=8}
|> Map.add "tried" { in_spam=13; in_ham=133}
|> Map.add "tries" { in_spam=0; in_ham=17}
|> Map.add "trigger" { in_spam=0; in_ham=10}
|> Map.add "trim" { in_spam=14; in_ham=3}
|> Map.add "trip" { in_spam=8; in_ham=12}
|> Map.add "trips" { in_spam=1; in_ham=7}
|> Map.add "trivial" { in_spam=0; in_ham=24}
|> Map.add "trouble" { in_spam=3; in_ham=50}
|> Map.add "troubles" { in_spam=9; in_ham=4}
|> Map.add "troy" { in_spam=0; in_ham=8}
|> Map.add "truck" { in_spam=7; in_ham=8}
|> Map.add "true" { in_spam=48; in_ham=116}
|> Map.add "truly" { in_spam=5; in_ham=24}
|> Map.add "trust" { in_spam=25; in_ham=29}
|> Map.add "trusted" { in_spam=4; in_ham=21}
|> Map.add "truth" { in_spam=9; in_ham=31}
|> Map.add "try" { in_spam=38; in_ham=226}
|> Map.add "trying" { in_spam=12; in_ham=164}
|> Map.add "tue" { in_spam=1; in_ham=96}
|> Map.add "tuesday" { in_spam=3; in_ham=71}
|> Map.add "tuning" { in_spam=0; in_ham=10}
|> Map.add "turn" { in_spam=26; in_ham=57}
|> Map.add "turned" { in_spam=2; in_ham=52}
|> Map.add "turning" { in_spam=0; in_ham=17}
|> Map.add "turns" { in_spam=9; in_ham=32}
|> Map.add "turpin" { in_spam=0; in_ham=24}
|> Map.add "tweaking" { in_spam=0; in_ham=11}
|> Map.add "twenty" { in_spam=8; in_ham=9}
|> Map.add "twice" { in_spam=1; in_ham=21}
|> Map.add "two" { in_spam=44; in_ham=257}
|> Map.add "txt" { in_spam=2; in_ham=20}
|> Map.add "tyler" { in_spam=0; in_ham=8}
|> Map.add "type" { in_spam=89; in_ham=107}
|> Map.add "typed" { in_spam=0; in_ham=9}
|> Map.add "types" { in_spam=7; in_ham=28}
|> Map.add "typical" { in_spam=3; in_ham=27}
|> Map.add "typically" { in_spam=1; in_ham=14}
|> Map.add "typing" { in_spam=2; in_ham=14}
|> Map.add "ucsc" { in_spam=0; in_ham=8}
|> Map.add "ugly" { in_spam=3; in_ham=24}
|> Map.add "ulises" { in_spam=0; in_ham=9}
|> Map.add "ultimate" { in_spam=9; in_ham=8}
|> Map.add "ultimately" { in_spam=1; in_ham=17}
|> Map.add "unable" { in_spam=1; in_ham=30}
|> Map.add "uname" { in_spam=0; in_ham=8}
|> Map.add "unblessed" { in_spam=0; in_ham=14}
|> Map.add "under" { in_spam=33; in_ham=152}
|> Map.add "underground" { in_spam=2; in_ham=7}
|> Map.add "underline" { in_spam=10; in_ham=3}
|> Map.add "understand" { in_spam=28; in_ham=85}
|> Map.add "understands" { in_spam=4; in_ham=8}
|> Map.add "understood" { in_spam=2; in_ham=18}
|> Map.add "underwriting" { in_spam=12; in_ham=41}
|> Map.add "union" { in_spam=2; in_ham=12}
|> Map.add "unique" { in_spam=12; in_ham=36}
|> Map.add "unit" { in_spam=4; in_ham=20}
|> Map.add "united" { in_spam=46; in_ham=53}
|> Map.add "universal" { in_spam=6; in_ham=10}
|> Map.add "universe" { in_spam=1; in_ham=17}
|> Map.add "university" { in_spam=5; in_ham=55}
|> Map.add "unix" { in_spam=0; in_ham=41}
|> Map.add "unknown" { in_spam=5; in_ham=26}
|> Map.add "unless" { in_spam=7; in_ham=77}
|> Map.add "unlike" { in_spam=6; in_ham=26}
|> Map.add "unlikely" { in_spam=0; in_ham=10}
|> Map.add "unparalleled" { in_spam=0; in_ham=10}
|> Map.add "unread" { in_spam=0; in_ham=11}
|> Map.add "unrelated" { in_spam=0; in_ham=12}
|> Map.add "unresponsive" { in_spam=0; in_ham=8}
|> Map.add "unseen" { in_spam=0; in_ham=41}
|> Map.add "unsolicited" { in_spam=34; in_ham=12}
|> Map.add "unstable" { in_spam=3; in_ham=8}
|> Map.add "unsubscribe" { in_spam=92; in_ham=101}
|> Map.add "unsubscribed" { in_spam=31; in_ham=2}
|> Map.add "until" { in_spam=21; in_ham=120}
|> Map.add "unusual" { in_spam=1; in_ham=23}
|> Map.add "unveiled" { in_spam=0; in_ham=11}
|> Map.add "unwanted" { in_spam=23; in_ham=1}
|> Map.add "update" { in_spam=4; in_ham=85}
|> Map.add "updated" { in_spam=6; in_ham=40}
|> Map.add "updates" { in_spam=8; in_ham=26}
|> Map.add "updating" { in_spam=0; in_ham=16}
|> Map.add "upgrade" { in_spam=2; in_ham=45}
|> Map.add "upgraded" { in_spam=0; in_ham=24}
|> Map.add "upgrades" { in_spam=1; in_ham=9}
|> Map.add "upgrading" { in_spam=0; in_ham=17}
|> Map.add "upon" { in_spam=21; in_ham=119}
|> Map.add "upstream" { in_spam=0; in_ham=8}
|> Map.add "urban" { in_spam=1; in_ham=12}
|> Map.add "urgent" { in_spam=14; in_ham=12}
|> Map.add "url" { in_spam=22; in_ham=672}
|> Map.add "urls" { in_spam=0; in_ham=9}
|> Map.add "us-ascii" { in_spam=0; in_ham=10}
|> Map.add "usa" { in_spam=19; in_ham=56}
|> Map.add "usability" { in_spam=0; in_ham=11}
|> Map.add "usage" { in_spam=3; in_ham=17}
|> Map.add "usb" { in_spam=1; in_ham=11}
|> Map.add "usd" { in_spam=11; in_ham=2}
|> Map.add "use" { in_spam=101; in_ham=603}
|> Map.add "used" { in_spam=38; in_ham=275}
|> Map.add "useful" { in_spam=3; in_ham=73}
|> Map.add "usefulness" { in_spam=0; in_ham=39}
|> Map.add "useless" { in_spam=0; in_ham=61}
|> Map.add "usenet" { in_spam=0; in_ham=8}
|> Map.add "user" { in_spam=28; in_ham=179}
|> Map.add "user's" { in_spam=0; in_ham=15}
|> Map.add "userid" { in_spam=9; in_ham=3}
|> Map.add "userland" { in_spam=0; in_ham=60}
|> Map.add "users" { in_spam=24; in_ham=97}
|> Map.add "users'" { in_spam=46; in_ham=92}
|> Map.add "uses" { in_spam=5; in_ham=57}
|> Map.add "using" { in_spam=52; in_ham=372}
|> Map.add "usr" { in_spam=0; in_ham=99}
|> Map.add "usual" { in_spam=0; in_ham=34}
|> Map.add "usually" { in_spam=5; in_ham=65}
|> Map.add "usw-pr-cvs" { in_spam=0; in_ham=16}
|> Map.add "utilities" { in_spam=22; in_ham=5}
|> Map.add "utility" { in_spam=3; in_ham=10}
|> Map.add "utter" { in_spam=2; in_ham=8}
|> Map.add "utterly" { in_spam=2; in_ham=7}
|> Map.add "vacation" { in_spam=5; in_ham=10}
|> Map.add "valhalla" { in_spam=0; in_ham=49}
|> Map.add "valid" { in_spam=20; in_ham=22}
|> Map.add "valign" { in_spam=92; in_ham=3}
|> Map.add "valley" { in_spam=4; in_ham=7}
|> Map.add "valuable" { in_spam=35; in_ham=13}
|> Map.add "value" { in_spam=92; in_ham=57}
|> Map.add "values" { in_spam=4; in_ham=20}
|> Map.add "van" { in_spam=0; in_ham=41}
|> Map.add "vanilla" { in_spam=0; in_ham=9}
|> Map.add "var" { in_spam=6; in_ham=26}
|> Map.add "variable" { in_spam=1; in_ham=21}
|> Map.add "variety" { in_spam=6; in_ham=24}
|> Map.add "various" { in_spam=5; in_ham=81}
|> Map.add "vary" { in_spam=2; in_ham=11}
|> Map.add "vast" { in_spam=13; in_ham=29}
|> Map.add "ved" { in_spam=0; in_ham=8}
|> Map.add "vehicle" { in_spam=3; in_ham=10}
|> Map.add "vehicles" { in_spam=6; in_ham=5}
|> Map.add "vendor" { in_spam=0; in_ham=24}
|> Map.add "vendors" { in_spam=0; in_ham=12}
|> Map.add "venture" { in_spam=15; in_ham=10}
|> Map.add "verdana" { in_spam=91; in_ham=3}
|> Map.add "verified" { in_spam=6; in_ham=5}
|> Map.add "verify" { in_spam=8; in_ham=15}
|> Map.add "version" { in_spam=15; in_ham=250}
|> Map.add "versions" { in_spam=1; in_ham=45}
|> Map.add "versus" { in_spam=1; in_ham=14}
|> Map.add "vertical" { in_spam=3; in_ham=7}
|> Map.add "very" { in_spam=69; in_ham=295}
|> Map.add "veteran" { in_spam=0; in_ham=10}
|> Map.add "via" { in_spam=32; in_ham=102}
|> Map.add "viable" { in_spam=1; in_ham=7}
|> Map.add "vice" { in_spam=0; in_ham=16}
|> Map.add "video" { in_spam=14; in_ham=24}
|> Map.add "videos" { in_spam=7; in_ham=7}
|> Map.add "view" { in_spam=21; in_ham=50}
|> Map.add "viewed" { in_spam=2; in_ham=9}
|> Map.add "viewing" { in_spam=6; in_ham=9}
|> Map.add "viewpoint" { in_spam=0; in_ham=9}
|> Map.add "views" { in_spam=1; in_ham=16}
|> Map.add "village" { in_spam=3; in_ham=9}
|> Map.add "ville" { in_spam=2; in_ham=24}
|> Map.add "vincent" { in_spam=2; in_ham=7}
|> Map.add "violate" { in_spam=0; in_ham=9}
|> Map.add "violence" { in_spam=1; in_ham=19}
|> Map.add "violent" { in_spam=1; in_ham=12}
|> Map.add "vipul" { in_spam=0; in_ham=9}
|> Map.add "vircio" { in_spam=0; in_ham=47}
|> Map.add "virgin" { in_spam=3; in_ham=7}
|> Map.add "virginia" { in_spam=7; in_ham=10}
|> Map.add "virtual" { in_spam=3; in_ham=20}
|> Map.add "virtually" { in_spam=8; in_ham=12}
|> Map.add "virus" { in_spam=1; in_ham=23}
|> Map.add "viruses" { in_spam=15; in_ham=17}
|> Map.add "visa" { in_spam=10; in_ham=3}
|> Map.add "visible" { in_spam=1; in_ham=18}
|> Map.add "vision" { in_spam=3; in_ham=14}
|> Map.add "visionary" { in_spam=2; in_ham=7}
|> Map.add "visions" { in_spam=0; in_ham=8}
|> Map.add "visit" { in_spam=78; in_ham=27}
|> Map.add "visited" { in_spam=15; in_ham=6}
|> Map.add "visiting" { in_spam=2; in_ham=16}
|> Map.add "visual" { in_spam=4; in_ham=8}
|> Map.add "vlink" { in_spam=26; in_ham=0}
|> Map.add "voice" { in_spam=3; in_ham=30}
|> Map.add "voices" { in_spam=0; in_ham=15}
|> Map.add "volume" { in_spam=5; in_ham=19}
|> Map.add "vote" { in_spam=1; in_ham=19}
|> Map.add "voters" { in_spam=0; in_ham=8}
|> Map.add "vous" { in_spam=3; in_ham=6}
|> Map.add "voyage" { in_spam=0; in_ham=8}
|> Map.add "vspace" { in_spam=19; in_ham=1}
|> Map.add "vulnerable" { in_spam=0; in_ham=11}
|> Map.add "waider" { in_spam=0; in_ham=9}
|> Map.add "wait" { in_spam=24; in_ham=54}
|> Map.add "waiting" { in_spam=25; in_ham=41}
|> Map.add "wake" { in_spam=1; in_ham=8}
|> Map.add "walk" { in_spam=9; in_ham=26}
|> Map.add "walking" { in_spam=0; in_ham=11}
|> Map.add "walks" { in_spam=7; in_ham=4}
|> Map.add "wall" { in_spam=7; in_ham=22}
|> Map.add "wan" { in_spam=0; in_ham=9}
|> Map.add "want" { in_spam=146; in_ham=308}
|> Map.add "wanted" { in_spam=17; in_ham=70}
|> Map.add "wanting" { in_spam=6; in_ham=9}
|> Map.add "wants" { in_spam=6; in_ham=67}
|> Map.add "war" { in_spam=9; in_ham=88}
|> Map.add "ward" { in_spam=0; in_ham=16}
|> Map.add "warm" { in_spam=1; in_ham=7}
|> Map.add "warming" { in_spam=0; in_ham=19}
|> Map.add "warned" { in_spam=1; in_ham=12}
|> Map.add "warning" { in_spam=4; in_ham=27}
|> Map.add "warnings" { in_spam=1; in_ham=11}
|> Map.add "warrant" { in_spam=3; in_ham=9}
|> Map.add "wars" { in_spam=1; in_ham=10}
|> Map.add "was" { in_spam=108; in_ham=739}
|> Map.add "washington" { in_spam=19; in_ham=36}
|> Map.add "wasn't" { in_spam=0; in_ham=76}
|> Map.add "waste" { in_spam=12; in_ham=20}
|> Map.add "watch" { in_spam=24; in_ham=24}
|> Map.add "watched" { in_spam=0; in_ham=12}
|> Map.add "watching" { in_spam=10; in_ham=38}
|> Map.add "water" { in_spam=1; in_ham=26}
|> Map.add "wave" { in_spam=1; in_ham=7}
|> Map.add "way" { in_spam=62; in_ham=401}
|> Map.add "ways" { in_spam=14; in_ham=62}
|> Map.add "we'd" { in_spam=0; in_ham=12}
|> Map.add "we'll" { in_spam=11; in_ham=35}
|> Map.add "we're" { in_spam=9; in_ham=86}
|> Map.add "we've" { in_spam=11; in_ham=54}
|> Map.add "weak" { in_spam=1; in_ham=18}
|> Map.add "wealth" { in_spam=10; in_ham=16}
|> Map.add "weapons" { in_spam=0; in_ham=16}
|> Map.add "wear" { in_spam=7; in_ham=8}
|> Map.add "wears" { in_spam=1; in_ham=7}
|> Map.add "weather" { in_spam=0; in_ham=23}
|> Map.add "web" { in_spam=68; in_ham=152}
|> Map.add "web's" { in_spam=11; in_ham=3}
|> Map.add "weblog" { in_spam=0; in_ham=45}
|> Map.add "weblogs" { in_spam=1; in_ham=20}
|> Map.add "webmaster" { in_spam=9; in_ham=11}
|> Map.add "website" { in_spam=46; in_ham=39}
|> Map.add "websites" { in_spam=17; in_ham=7}
|> Map.add "wed" { in_spam=2; in_ham=132}
|> Map.add "wedded" { in_spam=0; in_ham=10}
|> Map.add "wednesday" { in_spam=1; in_ham=54}
|> Map.add "week" { in_spam=40; in_ham=97}
|> Map.add "weekend" { in_spam=0; in_ham=24}
|> Map.add "weekly" { in_spam=13; in_ham=6}
|> Map.add "weeks" { in_spam=27; in_ham=42}
|> Map.add "weight" { in_spam=20; in_ham=7}
|> Map.add "weird" { in_spam=0; in_ham=33}
|> Map.add "weirdpage" { in_spam=0; in_ham=8}
|> Map.add "welch" { in_spam=0; in_ham=32}
|> Map.add "welcome" { in_spam=11; in_ham=130}
|> Map.add "welfare" { in_spam=5; in_ham=7}
|> Map.add "well" { in_spam=55; in_ham=321}
|> Map.add "went" { in_spam=7; in_ham=89}
|> Map.add "were" { in_spam=47; in_ham=339}
|> Map.add "weren't" { in_spam=3; in_ham=18}
|> Map.add "west" { in_spam=12; in_ham=33}
|> Map.add "western" { in_spam=4; in_ham=18}
|> Map.add "wet" { in_spam=3; in_ham=6}
|> Map.add "what" { in_spam=86; in_ham=668}
|> Map.add "what's" { in_spam=8; in_ham=71}
|> Map.add "whatever" { in_spam=8; in_ham=74}
|> Map.add "whatsoever" { in_spam=5; in_ham=16}
|> Map.add "wheel" { in_spam=0; in_ham=9}
|> Map.add "when" { in_spam=80; in_ham=626}
|> Map.add "whenever" { in_spam=0; in_ham=13}
|> Map.add "where" { in_spam=58; in_ham=322}
|> Map.add "whereas" { in_spam=1; in_ham=12}
|> Map.add "whether" { in_spam=24; in_ham=88}
|> Map.add "which" { in_spam=88; in_ham=617}
|> Map.add "while" { in_spam=49; in_ham=229}
|> Map.add "white" { in_spam=27; in_ham=34}
|> Map.add "whitehead" { in_spam=0; in_ham=17}
|> Map.add "whitelist" { in_spam=0; in_ham=18}
|> Map.add "who" { in_spam=157; in_ham=430}
|> Map.add "who's" { in_spam=0; in_ham=17}
|> Map.add "whole" { in_spam=23; in_ham=98}
|> Map.add "whom" { in_spam=3; in_ham=22}
|> Map.add "whose" { in_spam=13; in_ham=38}
|> Map.add "whump" { in_spam=0; in_ham=9}
|> Map.add "why" { in_spam=50; in_ham=248}
|> Map.add "wide" { in_spam=10; in_ham=25}
|> Map.add "widely" { in_spam=2; in_ham=25}
|> Map.add "widespread" { in_spam=0; in_ham=13}
|> Map.add "width" { in_spam=171; in_ham=9}
|> Map.add "wife" { in_spam=10; in_ham=20}
|> Map.add "wifi" { in_spam=0; in_ham=11}
|> Map.add "wild" { in_spam=6; in_ham=8}
|> Map.add "will" { in_spam=239; in_ham=560}
|> Map.add "william" { in_spam=1; in_ham=20}
|> Map.add "williams" { in_spam=9; in_ham=34}
|> Map.add "willing" { in_spam=19; in_ham=44}
|> Map.add "wilt" { in_spam=0; in_ham=8}
|> Map.add "win" { in_spam=9; in_ham=47}
|> Map.add "wind" { in_spam=3; in_ham=17}
|> Map.add "window" { in_spam=18; in_ham=67}
|> Map.add "windows" { in_spam=15; in_ham=85}
|> Map.add "windows-" { in_spam=47; in_ham=2}
|> Map.add "wink" { in_spam=1; in_ham=40}
|> Map.add "winner" { in_spam=2; in_ham=10}
|> Map.add "winning" { in_spam=7; in_ham=5}
|> Map.add "winter" { in_spam=0; in_ham=14}
|> Map.add "wipe" { in_spam=4; in_ham=6}
|> Map.add "wired" { in_spam=1; in_ham=17}
|> Map.add "wireless" { in_spam=3; in_ham=36}
|> Map.add "wisdom" { in_spam=0; in_ham=16}
|> Map.add "wise" { in_spam=0; in_ham=10}
|> Map.add "wish" { in_spam=173; in_ham=43}
|> Map.add "wishes" { in_spam=2; in_ham=9}
|> Map.add "with" { in_spam=305; in_ham=1213}
|> Map.add "within" { in_spam=73; in_ham=84}
|> Map.add "without" { in_spam=58; in_ham=220}
|> Map.add "witnesses" { in_spam=0; in_ham=8}
|> Map.add "wives" { in_spam=3; in_ham=6}
|> Map.add "woman" { in_spam=4; in_ham=39}
|> Map.add "women" { in_spam=10; in_ham=52}
|> Map.add "women's" { in_spam=2; in_ham=9}
|> Map.add "won" { in_spam=1; in_ham=23}
|> Map.add "won't" { in_spam=11; in_ham=91}
|> Map.add "wonder" { in_spam=5; in_ham=42}
|> Map.add "wondered" { in_spam=8; in_ham=13}
|> Map.add "wonderful" { in_spam=18; in_ham=27}
|> Map.add "wondering" { in_spam=2; in_ham=23}
|> Map.add "wood" { in_spam=1; in_ham=12}
|> Map.add "word" { in_spam=27; in_ham=85}
|> Map.add "words" { in_spam=4; in_ham=84}
|> Map.add "work" { in_spam=77; in_ham=332}
|> Map.add "workaround" { in_spam=0; in_ham=9}
|> Map.add "worked" { in_spam=9; in_ham=81}
|> Map.add "workers" { in_spam=1; in_ham=22}
|> Map.add "working" { in_spam=33; in_ham=167}
|> Map.add "works" { in_spam=29; in_ham=146}
|> Map.add "world" { in_spam=40; in_ham=286}
|> Map.add "world's" { in_spam=6; in_ham=107}
|> Map.add "worldwide" { in_spam=17; in_ham=22}
|> Map.add "worried" { in_spam=1; in_ham=16}
|> Map.add "worry" { in_spam=9; in_ham=25}
|> Map.add "worse" { in_spam=2; in_ham=46}
|> Map.add "worst" { in_spam=18; in_ham=38}
|> Map.add "worth" { in_spam=21; in_ham=86}
|> Map.add "worthwhile" { in_spam=5; in_ham=5}
|> Map.add "worthy" { in_spam=2; in_ham=7}
|> Map.add "would" { in_spam=99; in_ham=594}
|> Map.add "wouldn't" { in_spam=11; in_ham=67}
|> Map.add "wound" { in_spam=0; in_ham=8}
|> Map.add "wow" { in_spam=0; in_ham=22}
|> Map.add "wp-dyn" { in_spam=0; in_ham=12}
|> Map.add "write" { in_spam=25; in_ham=91}
|> Map.add "writer" { in_spam=3; in_ham=19}
|> Map.add "writers" { in_spam=1; in_ham=7}
|> Map.add "writes" { in_spam=0; in_ham=163}
|> Map.add "writing" { in_spam=11; in_ham=50}
|> Map.add "written" { in_spam=7; in_ham=62}
|> Map.add "wrong" { in_spam=12; in_ham=98}
|> Map.add "wrong-doers" { in_spam=0; in_ham=35}
|> Map.add "wrote" { in_spam=0; in_ham=784}
|> Map.add "wrt" { in_spam=0; in_ham=9}
|> Map.add "wtf" { in_spam=0; in_ham=11}
|> Map.add "www" { in_spam=312; in_ham=1413}
|> Map.add "x-mailer" { in_spam=1; in_ham=11}
|> Map.add "xent" { in_spam=0; in_ham=149}
|> Map.add "xim" { in_spam=0; in_ham=42}
|> Map.add "xine" { in_spam=0; in_ham=24}
|> Map.add "xml" { in_spam=3; in_ham=45}
|> Map.add "xmms" { in_spam=0; in_ham=16}
|> Map.add "xxx" { in_spam=10; in_ham=4}
|> Map.add "yahoo" { in_spam=19; in_ham=145}
|> Map.add "yahoogroups" { in_spam=0; in_ham=8}
|> Map.add "yannick" { in_spam=0; in_ham=13}
|> Map.add "ybb" { in_spam=1; in_ham=51}
|> Map.add "yea" { in_spam=1; in_ham=8}
|> Map.add "yeah" { in_spam=4; in_ham=49}
|> Map.add "year" { in_spam=64; in_ham=123}
|> Map.add "year's" { in_spam=4; in_ham=13}
|> Map.add "year-old" { in_spam=6; in_ham=25}
|> Map.add "years" { in_spam=66; in_ham=224}
|> Map.add "yep" { in_spam=2; in_ham=15}
|> Map.add "yes" { in_spam=53; in_ham=177}
|> Map.add "yesterday" { in_spam=0; in_ham=57}
|> Map.add "yet" { in_spam=18; in_ham=152}
|> Map.add "york" { in_spam=20; in_ham=44}
|> Map.add "you" { in_spam=413; in_ham=1198}
|> Map.add "you'd" { in_spam=9; in_ham=47}
|> Map.add "you'll" { in_spam=54; in_ham=72}
|> Map.add "you're" { in_spam=34; in_ham=206}
|> Map.add "you've" { in_spam=22; in_ham=52}
|> Map.add "young" { in_spam=4; in_ham=31}
|> Map.add "your" { in_spam=379; in_ham=646}
|> Map.add "yours" { in_spam=49; in_ham=8}
|> Map.add "yourself" { in_spam=48; in_ham=40}
|> Map.add "yup" { in_spam=0; in_ham=11}
|> Map.add "zawodny" { in_spam=0; in_ham=37}
|> Map.add "zealand" { in_spam=11; in_ham=3}
|> Map.add "zealot" { in_spam=0; in_ham=8}
|> Map.add "zero" { in_spam=5; in_ham=24}
|> Map.add "ziggy" { in_spam=0; in_ham=15}
|> Map.add "zip" { in_spam=15; in_ham=14}
|> Map.add "zone" { in_spam=2; in_ham=10}
|> Map.add "zope" { in_spam=2; in_ham=7}
|> Map.add "zzzz" { in_spam=44; in_ham=1}

