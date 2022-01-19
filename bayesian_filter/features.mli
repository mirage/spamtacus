module BayesianBody : Spamtacus.FEATURE with type db = Database.db
(** Naive Bayesian filter on mail body content. *)

module BayesianSubject : Spamtacus.FEATURE with type db = Database.db
(** Naive Bayesian filter on subject header value. *)

module BasicAntiVirus : Spamtacus.FEATURE
(** Basic antivirus that  rejects mails with forbidden attachment. It looks for
  attachment file name in  :
  {ul
  {- [name] parameter of [content-type] headers}
  {- [filename] parameter of [content-disposition] headers}}
    in each part of an incoming mail. If the attachement filenames found have 
  a forbidden extension  the mail is given a rank of [1.].
 
  Forbidden extensions are [.ade], [.adp], [.bat], [.chm], [.cmd],
   [.com], [.cpl], [.exe], [.hta], [.ins], [isp], [.jse], [.lib],
   [.mde], [.msc], [.msp], [.mst], [.pif], [.scr], [.sct], [shb],
   [.sys], [.vb], [.vbe], [.vbs], [.vxd], [.wsc], [.wsf], [.wsh].  

  Note: This feature does not require any machine learning algorithm nor
 a database. *)
