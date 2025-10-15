import { memo, useCallback } from "react";
import { Button, Card, Input } from "../../components";
import useRegistrationPage from "./useRegistrationPage";
import classes from "../Login/login.module.css";
import registrationClasses from "./registrationPage.module.css";

const RegistrationPage = () => {
  const {
    handleRegister,
    username,
    password,
    confirmPassword,
    errorMessage,
    isValidator,
    error,
    handleCheckboxChange,
  } = useRegistrationPage();

  return (
    <div className={classes.page}>
      <Card className={classes.card} title="Registration Page">
        {errorMessage && <label className={classes.errorLabel}>{errorMessage}</label>}
        {error && <label className={classes.errorLabel}>{error}</label>}
        <Input
          reference={username}
          label="username"
          className={classes.input}
          hasError={[
            { valid: (value: string) => value.length > 0, errorMessage: "field can't be empty" },
          ]}
          inputType={"text"}
          id="inp1"
        ></Input>
        <Input
          inputType={"password"}
          reference={password}
          label={"password"}
          className={classes.input}
          id="inp2"
          hasError={[
            {
              valid: (value: string) => value.length > 0,
              errorMessage: "Invalid password format",
            },
          ]}
        ></Input>
        <Input
          inputType={"password"}
          reference={confirmPassword}
          label={"confirm password"}
          className={classes.input}
          id="inp3"
          hasError={[
            {
              valid: useCallback((value: string) => value === password.current?.value, [password]),
              errorMessage: "Fields are not equal",
            },
          ]}
        ></Input>
        <div className={registrationClasses.checkbox}>
          <label className={`${isValidator ? registrationClasses.activeCheckbox : ""}`}>
            VALIDATOR SIGNUP
          </label>
          <input type={"checkbox"} onChange={(ev) => handleCheckboxChange(ev)} />
        </div>
        <Button text="Sign up" onClick={handleRegister} className={classes.loginButton}></Button>
      </Card>
    </div>
  );
};

export default memo(RegistrationPage);
