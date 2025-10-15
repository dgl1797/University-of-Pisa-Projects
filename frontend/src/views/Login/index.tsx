import React, { memo } from "react";
import { Button, Card, Input } from "../../components";
import classes from "./login.module.css";
import useLogin from "./useLogin";

const Login = () => {
  const { handleLogin, username, password, errorMessage, error } = useLogin();
  return (
    <div className={classes.page}>
      <Card title="LOGIN" className={classes.card}>
        {errorMessage && <label className={classes.errorLabel}>{errorMessage}</label>}
        {error && <label className={classes.errorLabel}>{error}</label>}
        <Input
          inputType={"text"}
          reference={username}
          label="username"
          className={classes.input}
          hasError={[
            { valid: (value: string) => value.length > 0, errorMessage: "field can't be empty" },
          ]}
        />
        <Input
          inputType={"password"}
          reference={password}
          label={"password"}
          className={classes.input}
          hasError={[
            {
              valid: (value: string) => value.length > 0,
              errorMessage: "Invalid password format",
            },
          ]}
        />
        <Button text="Log in" onClick={handleLogin} className={classes.loginButton} />
      </Card>
    </div>
  );
};

export default memo(Login);
