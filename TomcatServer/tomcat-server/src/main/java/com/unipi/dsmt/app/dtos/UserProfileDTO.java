package com.unipi.dsmt.app.dtos;

public class UserProfileDTO {
    private String username;
    private String name;
    private String surname;
    private Boolean online_flag;
    private String department;

    public UserProfileDTO(String username, String name, String surname, Boolean online_flag, String department) {
        this.username = username;
        this.name = name;
        this.surname = surname;
        this.online_flag = online_flag;
        this.department = department;
    }

    public String getUsername() {
        return this.username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getSurname() {
        return this.surname;
    }

    public void setSurname(String surname) {
        this.surname = surname;
    }

    public Boolean isOnline_flag() {
        return this.online_flag;
    }

    public Boolean getOnline_flag() {
        return this.online_flag;
    }

    public void setOnline_flag(Boolean online_flag) {
        this.online_flag = online_flag;
    }

    public String getDepartment() {
        return this.department;
    }

    public void setDepartment(String department) {
        this.department = department;
    }

    @Override
    public String toString() {
        return "{" +
            " username='" + getUsername() + "'" +
            ", name='" + getName() + "'" +
            ", surname='" + getSurname() + "'" +
            ", online_flag='" + isOnline_flag() + "'" +
            ", department='" + getDepartment() + "'" +
            "}";
    }

}
