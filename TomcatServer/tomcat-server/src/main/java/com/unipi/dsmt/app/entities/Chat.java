package com.unipi.dsmt.app.entities;

import java.sql.Date;

public class Chat {
    private String user1;
    private String user2;
    private Date creationTime;

    public Chat(String user1, String user2, Date creationTime) {
        this.user1 = user1;
        this.user2 = user2;
        this.creationTime = creationTime;
    }

    public String getUser1() {
        return this.user1;
    }

    public void setUser1(String user1) {
        this.user1 = user1;
    }

    public String getUser2() {
        return this.user2;
    }

    public void setUser2(String user2) {
        this.user2 = user2;
    }

    public Date getCreationTime() {
        return this.creationTime;
    }

    public void setCreationTime(Date creationTime) {
        this.creationTime = creationTime;
    }

    @Override
    public String toString() {
        return "{" +
            " user1='" + getUser1() + "'" +
            ", user2='" + getUser2() + "'" +
            ", creationTime='" + getCreationTime() + "'" +
            "}";
    }

}
