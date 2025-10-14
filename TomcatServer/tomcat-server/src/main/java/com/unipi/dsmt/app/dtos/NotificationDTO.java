package com.unipi.dsmt.app.dtos;

public class NotificationDTO {
    private String sender;
    private int chatID;
    private int count;

    public NotificationDTO(String sender, int chatID, int count) {
        this.sender = sender;
        this.chatID = chatID;
        this.count = count;
    }

    public int getCount() {
        return this.count;
    }

    public void setCount(int newCount) {
        this.count = newCount;
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

    @Override
    public String toString() {
        return "{" +
                " sender='" + getSender() + "'" +
                ", chatID='" + getChatID() + "'" +
                "}";
    }

}
