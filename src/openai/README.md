# OpenAI API example

A work in progress ...

## setting your OpenAI API key
 
 Define the  "OPENAI_KEY" environment variable with the value of your OpenAI API key
 
## Examples:

cl-user> (openai:completions "The President went to Congress" 200)
" instead. The Congress was not oblivious of what the Supreme Court's
majority had ruled. Even four Justices had found nothing to criticize
in the President's requirement that the Federal Government's four-year
contract with Microsoft be extended by twice as much, that the burden
of proof be put on Microsoft to show, not a violation, but that it
should be allowed to compete in any open industry, that the
combination of a powerful operating system with a powerful PC
operating system also give to Microsoft a PC-compatible
interface-vendor neutral, the Court called it- which other operating
systems could match, based on progress and development of time. Why
should that not be the case? Should the Court declare that any
operating system which antedated Microsoft's dominant operating system
would not be free to enter the market? The Justices on the Court, the
Chief Justice included, found it an extraordinarily tough question.\"
In a minority decision, Chief Justice Rehnquist, for instance, writes
this: \"Justice Kennedy's"

cl-user> (openai:answer-question "What rivers flow in Arizona?" 60)
"The Colorado, the Gila, the Little Colorado, the Salt, the Verde, the
San Pedro, the Santa Cruz, the San Juan, the Little Colorado, the Rio
Grande, the Gila, the San Pedro, the Santa Cruz, the San Juan, the
Little Colorado, the Rio"

cl-user>  (openai:summarize "upiter is the fifth planet from the Sun and the largest in the Solar System. It is a gas giant with a mass one-thousandth that of the Sun, but two-and-a-half times that of all the other planets in the Solar System combined. Jupiter is one of the brightest objects visible to the naked eye in the night sky, and has been known to ancient civilizations since before recorded history. It is named after the Roman god Jupiter.[19] When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows,[20] and is on average the third-brightest natural object in the night sky after the Moon and Venus." 40)
"The planet is usually the fourth-brightest object in the night sky, after the Sun, the Moon, and Venus.
Jupiter is a gas giant because it is predominantly composed of hydrogen"

