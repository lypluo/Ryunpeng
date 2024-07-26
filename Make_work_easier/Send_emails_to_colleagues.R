##########################################
#Aim: Try to send the mass emails to colleagues 
##########################################
#referring to the website:
#Chinese: https://blog.csdn.net/weixin_42679482/article/details/119700667
#English: https://mailtrap.io/blog/r-send-email/

##currently, smtp setting does not work-->so I could not use this function


library(rJava)
#-------------------
#1) R 语言程序运行结束的提示邮件
#-------------------
library(mailR)
body="Your successfully sent the email"
sender="lypcage@gmail"
recipients<-"yunpeng.luo@wsl.ch"
title="information prompt"
#sending the email:
#!comments: does not work!!
send.mail(from = sender, to = recipients,subject = title,
          body=body,
          html = T,
          encoding = "utf-8",
          smtp = list(host.name="smtp.gmail.com",port=465,
                    user.name=sender,passwd="BGC196229.lyp",ssl=TRUE),
          authenticate = TRUE,
          send = TRUE)


###sending to many people

# 1. Sender
sender <- "x@me.com"

# 2. Recipients
recipients <- c("y@you.com", "z@our.com")

# 3. Attached files
list_files_attached_location <- c("mtcars.csv")
list_files_attached_names <- c("mtcars name")
list_files_attached_description <- c("mtcars desc")

# 4. Send email
tryCatch({
  send.mail(from = sender,
            to = recipients,
            subject = "Your subject",
            body = "Your mail body",
            smtp = list(
              host.name = "smtp.gmail.com", 
              port = 465,
              user.name = sender,
              passwd = "psw", 
              ssl = TRUE),
            authenticate = TRUE,
            send = TRUE,
            attach.files = list_files_attached_location,
            file.names = list_files_attached_names,
            file.descriptions = list_files_attached_description,
            debug = TRUE
  )
}, 
error = function(e) {
  print(e)
  stop()
})
