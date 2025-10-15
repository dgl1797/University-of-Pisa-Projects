import { useCallback, useRef, useState } from "react";
import { useDispatch } from "react-redux";
import { useNavigate } from "react-router-dom";
import { useAPIs } from "../../hooks";
import { actions } from "../../store/reducers";

const useLogin = () => {
  const username = useRef<HTMLInputElement>(null);
  const password = useRef<HTMLInputElement>(null);
  const { callOnce, error } = useAPIs();
  const dispatch = useDispatch();
  const navigate = useNavigate();
  const [errorMessage, setErrorMessage] = useState<string>("");

  const handleRequest = useCallback(
    (data) => {
      dispatch(actions.session.setSession(data));
      navigate("/dashboard");
    },
    [dispatch, navigate]
  );

  const handleLogin = useCallback(() => {
    if (username.current?.value.match(/[a-z]+/gi) && (password.current?.value?.length ?? 0) > 0) {
      const formData = { username: username.current.value, password: password?.current?.value };
      callOnce("GET", "/auth", {
        callback: handleRequest,
        config: {
          params: { new: false },
          headers: {
            Authorization: `${formData.username}:${formData?.password?.replace(/:/g, "") ?? ":"}`,
          },
        },
      });
    } else {
      setErrorMessage("invalid username or password");
      setTimeout(() => setErrorMessage(""), 3000);
    }
  }, [callOnce, handleRequest]);
  return {
    username,
    password,
    handleLogin,
    errorMessage,
    error,
  };
};

export default useLogin;
