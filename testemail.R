library(tidyverse)
#install.packages("emayili") # or remotes::install_github("datawookie/emayili")
library(emayili)
SMTP_SERVER   = "smtp.gmail.com"
SMTP_PORT     = 465  # Gmail SMTP port (TLS): 587.SMTP port (SSL): 465. SMTP TLS/SSL required: yes.
ACCENTED_ATTACHMENT <- "dahlia.png"

smtp <- server(host = SMTP_SERVER, port = SMTP_PORT)

# Create email and add attachment.
#
email <- envelope( from = "cmpenr@gmail.com",
                     to = "adrmkir@yahoo.in",
                subject = "testing emayil sender"
                 )      #  %>%  attachment(ACCENTED_ATTACHMENT)

# Send it!
smtp(email)

# Create email and specify the sender.
email <- envelope(  to      = "bob@gmail.com",
                    subject = "Hi"
               ) %>%
       text("Hi Bob, it's Alice. I asked Craig to send this email.") %>%

        sender("craig@gmail.com")            %>%         # Craig is sending the message...

         from("alice@gmail.com")   # ... but it's actually for from Alice.
# Send it!
smtp(email)
