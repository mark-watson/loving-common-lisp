# Usage:

```lisp
(load "describe-image.lisp")
(describe-image:image-to-text "ticket.png" "Print out the plain text in this image")
```

Example output:

```
* (describe-image:image-to-text "ticket.png" "Print out the text in this image")
"Fanfares and Fireworks
Flagstaff Symphony Orchestra
Ardrey Memorial Auditorium
Friday, September 26, 2025
7:30 PM (AZ)

Level Section Row Seat
Main Main Level M 31

WJJNBY.1.2406.1498
Friday, September 26, 2025 @ 7:30 PM

Price Service Fee Ticket Type
$53.00 $0.00 Early Bird Tickets
New Subscriber C3

The unique barcodes on this ticket allow only one entry to the event. If multiple copies of an ETTicket are made, the first copy of the ETTicket to arrive at the event will gain entry after scanning and validation. Other copies of this ticket will be denied entry."
* 
```

