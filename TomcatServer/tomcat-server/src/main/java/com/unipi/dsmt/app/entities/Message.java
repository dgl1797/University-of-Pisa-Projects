package com.unipi.dsmt.app.entities;

import java.sql.Timestamp;

public class Message {
    private String content;
    private String sender;
    private Timestamp creationTime;
    private int chatID;

    public Message(String content, String sender, Timestamp creationTime, int chatID) {
        this.content = content;
        this.sender = sender;
        this.creationTime = creationTime;
        this.chatID = chatID;
    }

    public String getContent() {
        return this.content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public String getSender() {
        return this.sender;
    }

    public void setSender(String sender) {
        this.sender = sender;
    }

    public Timestamp getCreationTime() {
        return this.creationTime;
    }

    public void setCreationTime(Timestamp creationTime) {
        this.creationTime = creationTime;
    }

    public int getChatID() {
        return this.chatID;
    }

    public void setChatID(int chatID) {
        this.chatID = chatID;
    }

}
