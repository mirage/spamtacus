open Database
open BayesianBody_012
let db = db 
|> Map.add "younger" { in_spam=3; in_ham=3}
|> Map.add "your" { in_spam=379; in_ham=646}
|> Map.add "your's" { in_spam=1; in_ham=2}
|> Map.add "your-name" { in_spam=0; in_ham=3}
|> Map.add "yours" { in_spam=49; in_ham=8}
|> Map.add "yourself" { in_spam=48; in_ham=40}
|> Map.add "youth" { in_spam=3; in_ham=3}
|> Map.add "ytyv" { in_spam=0; in_ham=4}
|> Map.add "yuck" { in_spam=0; in_ham=5}
|> Map.add "yukon" { in_spam=1; in_ham=2}
|> Map.add "yup" { in_spam=0; in_ham=11}
|> Map.add "yyboc" { in_spam=0; in_ham=3}
|> Map.add "yyyy" { in_spam=0; in_ham=4}
|> Map.add "zaire" { in_spam=5; in_ham=0}
|> Map.add "zambia" { in_spam=2; in_ham=3}
|> Map.add "zawodny" { in_spam=0; in_ham=37}
|> Map.add "zdnet" { in_spam=0; in_ham=3}
|> Map.add "zealand" { in_spam=11; in_ham=3}
|> Map.add "zealot" { in_spam=0; in_ham=8}
|> Map.add "zealous" { in_spam=0; in_ham=3}
|> Map.add "zen" { in_spam=0; in_ham=6}
|> Map.add "zero" { in_spam=5; in_ham=24}
|> Map.add "zero-sum" { in_spam=0; in_ham=4}
|> Map.add "ziggy" { in_spam=0; in_ham=15}
|> Map.add "zimbabwe" { in_spam=6; in_ham=3}
|> Map.add "zip" { in_spam=15; in_ham=14}
|> Map.add "zipped" { in_spam=0; in_ham=3}
|> Map.add "znion" { in_spam=0; in_ham=5}
|> Map.add "zone" { in_spam=2; in_ham=10}
|> Map.add "zones" { in_spam=0; in_ham=6}
|> Map.add "zoo" { in_spam=1; in_ham=2}
|> Map.add "zope" { in_spam=2; in_ham=7}
|> Map.add "ztvn" { in_spam=0; in_ham=4}
|> Map.add "zzzz" { in_spam=44; in_ham=1}

