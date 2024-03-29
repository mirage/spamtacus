# Spam Filter State of the Art

Email filters are used to manage incoming emails in order to detect and
eliminate emails that contain spam or malicious code such as viruses, Trojans,
or malware. Spam filtering is the security mechanism employed by email services
at different layers of the network (in firewalls, email servers, email clients,
or email user interface) to block unsolicited or suspicious emails.

The first email spam filter MAPS (Mail Abuse Prevention System) appeared in
1996 in the shape of a list called RBL (Real-time Blackhole List) containing IP
addresses identified as being used for sending spam. MAPS is currently
functioning as a part of [Trend Micro Email Reputation Services][trendmicro].
Following the MAPS initiative, the email providers designed various mechanisms
for email spam filters to protect from the dangers posed by phishing,
email-borne malware, or ransomware. These mechanisms are used to decide the
risk level of each incoming email. Examples of such mechanisms include
satisfactory spam limits, sender policy frameworks, whitelists and blacklists,
sender reputation labelling and recipient verification tools.

The two common approaches used for filtering spam emails are knowledge
engineering and machine learning. Namely, emails are classified as either spam
or ham using a set of rules in knowledge engineering. Machine learning uses the
knowledge engineering results in the model training phase for the
classification of the training samples. Email spam filters employ various
machine learning algorithms such as Naïve Bayes, Support Vector Machines,
Neural Networks, Deep Learning, Decision Trees, Ensemble Classifiers, K-Nearest
Neighbour, Rough sets, or Random Forests. Next we present the spam filters
employed by the topmost email services: Outlook, Yahoo, Gmail.

Google's data centers make use of hundreds of weighted spam features to
calculate the likelihood for an email to be a spam. The spam likelihood formula
employed by Gmail uses state of the art spam detection machine learning
algorithms such as natural language processing, logistic regression, neural
networks, or optical character recognition for spam images. Moreover, machine
learning algorithms are developed to combine and rank large sets of Google
search results and users feedback allow Gmail to link hundreds of factors to
improve their spam classification, from the formatting of an email to the time
of day it is sent. To harness the amount of information, Google is using its
in-house open source machine learning framework, [TensorFlow][tensorflow], to
help train additional spam filters for Gmail users.
[TensorFlow][tensorflow-github] makes managing large data easier, while the
open-source nature of the framework means new research from the community can
be quickly integrated. With the new filters in place as of 2020, [Gmail
claims][gmail] to block now an additional 100 million spam messages every day
containing linked images, hidden embedded content, or being sent from newly
created domains that try to hide a low volume of spam messages within
legitimate traffic.

Yahoo mail uses several algorithms and a combination of methods rooted in basic
pattern matching techniques in the form of blacklisting, based on a complaint
feedback loop service, and whitelisting in the form of a user's list of trusted
correspondents. Moreover, Yahoo’s spam filters use reputation labelling that
take into account factors such as: IP address, URL, Domain, Sender, Autonomous
System Number (ASN), DKIM signatures, or DMARC authentication. We note that
Yahoo keeps confidential the reputation labelling methods so we could only
speculate that machine learning algorithms such as decision trees may be
employed. The fact of the matter is that Yahoo announces having [user accounts
monitoring][yahoo] mechanisms looking for suspicious activity, including from
government-backed actors.  This may as well be a consequence of two incidents
in 2016 when reports of Yahoo complying with a [government
request][government-request] to scan user accounts to identify certain
suspicious patterns were followed by yet another [data breach
attack][data-breach] when more than 150000 U.S. government employees personal
informations were stolen.

Outlook webmail service allows the users to send and receive emails in their
web browser, connect cloud storage services to their account, and secure the
email confidentiality via encryption or by disallowing the recipient's
forwarding option. Outlook has its own distinctive methods of filtering email
spams. The results of an [experiment available online][spam-bias] show that
Outlook classifies spam according to keywords present in the email. Microsoft
declined to comment but it is likely that a machine learning algorithm
identified specific keywords as a strong discriminator between spam and ham
messages. Besides the internal anti-spam mechanism, the Outlook users or system
administrators are [encouraged][outlook] to add dedicated spam filters such as
[SpamBully][spambully], which comes with a Bayesian ranking among other
filtering mechanisms.

In conclusion, machine learning algorithms have been extensively applied in
spam filtering. However, when these algorithms are trained on poisoned data, it
makes them a target for different attacks on the [reliability and accessibility
of emails][attack-ml]. The main glitch speculated by the attackers for
poisoning is the regular re-training phase that uses new instances of
undesirable activities. Nevertheless, real world systems such as financial
fraud, or credit card fraud as well as spam detection systems make use of the
poisoned data supplied by the attackers to detect attack sources by various
methods such as Bayesian deep neural networks proposed by [Intel
Labs][attack-intel-labs].

[trendmicro]: https://www.ers.trendmicro.com
[tensorflow]: https://www.tensorflow.org
[tensorflow-github]: https://github.com/tensorflow/tensorflow
[gmail]: https://cloud.google.com/blog/products/g-suite/ridding-gmail-of-100-million-more-spam-messages-with-tensorflow
[yahoo]: https://help.yahoo.com/kb/SLN26995.html
[government-request]: https://www.nytimes.com/2016/10/06/technology/yahoo-email-tech-companies-government-investigations.html
[data-breach]: https://www.datacenterknowledge.com/archives/2016/12/15/stolen-yahoo-data-includes-government-employee-information
[spam-bias]: https://github.com/algorithmwatch/spam-bias/
[outlook]: https://www.lifewire.com/top-anti-spam-plugins-outlook-1173636
[spambully]: https://spambully.com
[attack-ml]: https://people.eecs.berkeley.edu/~tygar/papers/SML/Spam_filter.pdf
[attack-intel-labs]: http://bayesiandeeplearning.org/2019/papers/112.pdf
