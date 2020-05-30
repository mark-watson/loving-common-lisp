This is the stub README.txt for the "kbnlp" project.

try:

````
(ql:quickload "categorize_summarize")
(in-package :categorize_summarize)

(defvar x "President Bill Clinton ran for president of the USA in two elections. George W Bush also ran twice. Bill Clinton took a long vacation in Europe to visit his daughter. Bill Clinton said that banking in Europe is a good business. The Euro is strong and account balances are up. Interest rates are remaining steady. The question is whether or not the US dollar remains the world's reserve currency - if not, the US economy will face a depression. In their zeal to protect their members from politically hazardous votes on issues such as gay marriage and gun control, Democrats running the House of Representatives are taking extraordinary steps to muzzle Republicans in this summer's debates on spending bills.

On Thursday, for example, Republicans had hoped to force debates on abortion, school vouchers and medical marijuana, as well as gay marriage and gun control, as part of House consideration of the federal government's contribution to the District of Columbia's city budget. The group went to Florida and Alaska.

No way, Democrats said.

At issue are 12 bills totaling more than $1.2 trillion in annual appropriations bills for funding most government programs — usually low-profile legislation that typically dominates the work of the House in June and July. For decades, those bills have come to the floor under an open process that allows any member to try to amend them. Often those amendments are an effort to change government policy by adding or subtracting money for carrying it out. Relentlessly rising unemployment is triggering more home foreclosures, threatening the Obama administration's efforts to end the housing crisis and diminishing hopes the economy will rebound with vigor.

In past recessions, the housing industry helped get the economy back on track. Home builders ramped up production, expecting buyers to take advantage of lower prices and jump into the market. But not this time.

These days, homeowners who got fixed-rate prime mortgages because they had good credit can't make their payments because they're out of work. That means even more foreclosures and further declines in home values.

The initial surge in foreclosures in 2007 and 2008 was tied to subprime mortgages issued during the housing boom to people with shaky credit. That crisis has ebbed, but it has been replaced by more traditional foreclosures tied to the recession.

Unemployment stood at 9.5 percent in June ")

(defvar words1 (myutils:words-from-string x))

(setq cats1 (categorize words1))


(summarize words1 cats1)
````

## Making a standalone executable (just one file):

````
$ sbcl
* (ql:quickload "kbnlp")
* (defun test123 () (print (kbnlp:make-text-object "President Bill Clinton ran for president of the USA")))
* (sb-ext:save-lisp-and-die "kbnlptest" :toplevel #'test123 :executable t)


