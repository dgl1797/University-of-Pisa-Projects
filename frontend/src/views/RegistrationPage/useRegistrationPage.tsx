import { ChangeEvent, useCallback, useRef, useState } from "react";
import { useDispatch } from "react-redux";
import { useNavigate } from "react-router-dom";
import { useAPIs } from "../../hooks";
import { actions } from "../../store/reducers";

const useRegistrationPage = () => {
  const username = useRef<HTMLInputElement>(null);
  const password = useRef<HTMLInputElement>(null);
  const confirmPassword = useRef<HTMLInputElement>(null);
  const [isValidator, setValidator] = useState<boolean>(false);
  const [errorMessage, setErrorMessage] = useState<string>("");
  const dispatch = useDispatch();
  const navigate = useNavigate();
  const { callOnce, error } = useAPIs();

  const handleCheckboxChange = useCallback((ev: ChangeEvent<HTMLInputElement>) => {
    setValidator(ev.target.checked);
  }, []);

  const handleRequest = useCallback(
    (data) => {
      dispatch(actions.session.setSession(data));
      navigate("/dashboard");
    },
    [dispatch, navigate]
  );

  const handleRegister = useCallback(() => {
    if (
      (username.current?.value.length ?? 0) > 0 &&
      (password.current?.value?.length ?? 0) > 0 &&
      confirmPassword.current?.value === password?.current?.value
    ) {
      const formData = {
        username: username.current?.value,
        password: password?.current?.value ?? ":",
        isValidator,
      };
      callOnce("GET", "/auth", {
        callback: handleRequest,
        config: {
          params: { new: true },
          headers: {
            Authorization: `${formData.username}:${formData.password}:${formData.isValidator}`,
          },
        },
      });
    } else {
      setErrorMessage("invalid username or password");
      setTimeout(() => setErrorMessage(""), 3000);
    }
  }, [username, password, confirmPassword, isValidator, callOnce, handleRequest]);
  return {
    username,
    password,
    confirmPassword,
    handleRegister,
    errorMessage,
    isValidator,
    handleCheckboxChange,
    error,
  };
};

export default useRegistrationPage;
