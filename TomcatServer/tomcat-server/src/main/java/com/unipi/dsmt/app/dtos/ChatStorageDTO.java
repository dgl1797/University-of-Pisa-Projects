package com.unipi.dsmt.app.dtos;

import java.sql.Date;

public class ChatStorageDTO {
    int id_chat;
    String username;
    Date lastMessageTime;
    boolean onlineState;

    public ChatStorageDTO(int id_chat, String username, Date lastMessageTime, boolean isOnline) {
        this.id_chat = id_chat;
        this.username = username;
        this.lastMessageTime = lastMessageTime;
        this.onlineState = isOnline;
    }

    public int getId_chat() {
        return this.id_chat;
    }

    public void setId_chat(int id_chat) {
        this.id_chat = id_chat;
    }

    public String getUsername() {
        return this.username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public Date getLastMessageTime() {
        return this.lastMessageTime;
    }

    public void setLastMessageTime(Date lastMessageTime) {
        this.lastMessageTime = lastMessageTime;
    }

    public boolean isOnlineState() {
        return this.onlineState;
    }

    public boolean getOnlineState() {
        return this.onlineState;
    }

    public void setOnlineState(boolean onlineState) {
        this.onlineState = onlineState;
    }

}
