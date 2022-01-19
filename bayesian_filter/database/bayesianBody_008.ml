open Database
open BayesianBody_007

let db =
  db
  |> Map.add "yielding" { in_spam = 0; in_ham = 6 }
  |> Map.add "york" { in_spam = 20; in_ham = 44 }
  |> Map.add "you" { in_spam = 414; in_ham = 1198 }
  |> Map.add "you'd" { in_spam = 9; in_ham = 47 }
  |> Map.add "you'll" { in_spam = 54; in_ham = 72 }
  |> Map.add "you're" { in_spam = 34; in_ham = 206 }
  |> Map.add "you've" { in_spam = 22; in_ham = 52 }
  |> Map.add "young" { in_spam = 4; in_ham = 31 }
  |> Map.add "your" { in_spam = 380; in_ham = 646 }
  |> Map.add "yours" { in_spam = 49; in_ham = 8 }
  |> Map.add "yourself" { in_spam = 48; in_ham = 40 }
  |> Map.add "yuck" { in_spam = 0; in_ham = 5 }
  |> Map.add "yup" { in_spam = 0; in_ham = 11 }
  |> Map.add "zawodny" { in_spam = 0; in_ham = 37 }
  |> Map.add "zealand" { in_spam = 11; in_ham = 3 }
  |> Map.add "zealot" { in_spam = 0; in_ham = 8 }
  |> Map.add "zen" { in_spam = 0; in_ham = 6 }
  |> Map.add "zero" { in_spam = 5; in_ham = 24 }
  |> Map.add "ziggy" { in_spam = 0; in_ham = 15 }
  |> Map.add "zimbabwe" { in_spam = 6; in_ham = 3 }
  |> Map.add "zip" { in_spam = 15; in_ham = 14 }
  |> Map.add "znion" { in_spam = 0; in_ham = 5 }
  |> Map.add "zone" { in_spam = 2; in_ham = 10 }
  |> Map.add "zones" { in_spam = 0; in_ham = 6 }
  |> Map.add "zope" { in_spam = 2; in_ham = 7 }
  |> Map.add "zzzz" { in_spam = 45; in_ham = 1 }
