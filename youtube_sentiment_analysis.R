library(tuber)
# Authenticate the youtube API

id<-"212849804854-n79dmp17b7rd2jjbi4vr29837um9vk2v.apps.googleusercontent.com"
client_secret<-"CjFLkNL2JIZQRZV9eE-pov3q"

yt_oauth(id,
         client_secret,token = "")
yt_master_data<-yt_search("Starbucks")

write.table(yt_master_data, file = "youttube_starbucks_masterdata.csv",row.names=FALSE,col.names=TRUE, sep=",")

master_data<-read.csv("youttube_starbucks_masterdata.csv",stringsAsFactors = FALSE)

videodata_ith<-get_all_comments(video_id = master_data$video_id[1],lang='en')

names(videodata_ith)

video_channel_ith<-get_all_channel_video_stats(channel_id = master_data$channelId[1])
write.table(video_channel_ith, file = "video_channel_ith.csv",row.names=FALSE,col.names=TRUE, sep=",")

names(video_channel_ith)

head(video_channel_ith)
