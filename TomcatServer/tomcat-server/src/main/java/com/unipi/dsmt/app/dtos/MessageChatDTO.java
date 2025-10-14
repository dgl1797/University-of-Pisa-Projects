package com.unipi.dsmt.app.dtos;

import java.sql.Timestamp;

public class MessageChatDTO {
    String content;
    String sender;
    Timestamp creationTime;

    public MessageChatDTO(String content, String sender, Timestamp creationTime) {
        this.content = content;
        this.sender = sender;
        this.creationTime = creationTime;
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

    @Override
    public String toString() {
        return "{" +
            " content='" + getContent() + "'" +
            ", sender='" + getSender() + "'" +
            ", creationTime='" + getCreationTime() + "'" +
            "}";
    }

}
