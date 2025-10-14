package com.unipi.dsmt.app.entities;

import java.sql.Timestamp;

public class Notification {
    private String user;
    private String sender;
    private int chatID;
    private Timestamp creationTime;

    public Notification(String user, String sender, int chatID, Timestamp creationTime) {
        this.user = user;
        this.sender = sender;
        this.chatID = chatID;
        this.creationTime = creationTime;
    }

    public String getUser() {
        return this.user;
    }

    public void setUser(String user) {
        this.user = user;
    }

    public String getSender() {
        return this.sender;
    }

    public void setSender(String sender) {
        this.sender = sender;
    }

    public int getChatID() {
        return this.chatID;
    }

    public void setChatID(int chatID) {
        this.chatID = chatID;
    }

    public Timestamp getCreationTime() {
        return this.creationTime;
    }

    public void setCreationTime(Timestamp creationTime) {
        this.creationTime = creationTime;
    }

    @Override
    public String toString() {
        return "{" +
            " user='" + getUser() + "'" +
            ", sender='" + getSender() + "'" +
            ", chatID='" + getChatID() + "'" +
            ", creationTime='" + getCreationTime() + "'" +
            "}";
    }

}
