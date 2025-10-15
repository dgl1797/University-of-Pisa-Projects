import { useCallback, useEffect, useRef, useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import { useNavigate } from "react-router-dom";
import { useAPIs } from "../../hooks";
import { actions, selectors } from "../../store/reducers";

const useAccount = () => {
  const userData = useSelector(selectors.user.getUserData);
  const userInfo = useSelector(selectors.user.getUserInfo);
  const token = useSelector(selectors.session.getSessionToken);
  const navigate = useNavigate();
  const usernameInput = useRef<HTMLInputElement>(null);
  const [editedUsername, setUsername] = useState<string | undefined>(userData?.username);
  const [edit, setEdit] = useState<boolean>(false);
  const dispatch = useDispatch();
  const { call, callOnce, error } = useAPIs();

  const handleGet = useCallback(
    (data) => {
      dispatch(actions.user.setItem(data?.replyData?.user ?? undefined));
      dispatch(
        actions.user.setStats({
          status: data?.replyData?.user?.isValidator ? "validator" : "common",
          givenLikes: data?.replyData?.likesCount,
          numberOfPrivate: data?.replyData?.numberOfPrivate,
          numberOfPublic: data?.replyData?.numberOfPublic,
          popularityScore:
            typeof data?.replyData?.popularityScore === "number"
              ? data?.replyData?.popularityScore
              : 0,
          bestContributor: {
            username: data?.replyData?.bestContributor?.username ?? "",
            contribution: data?.replyData?.bestContributor?.contribution ?? 0,
          },
        })
      );
    },
    [dispatch]
  );
  const saveUser = useCallback(
    (data) => {
      dispatch(actions.session.setSession({ token: data.newToken }));
      dispatch(actions.user.setItem(data.updatedUser));
      navigate(`/dashboard/${data.updatedUser._id}`);
    },
    [dispatch, navigate]
  );
  const handleSave = useCallback(() => {
    if (userData && token)
      callOnce("PUT", `/users/${userData._id}`, {
        callback: saveUser,
        payload: {
          username: editedUsername,
        },
        config: {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        },
      });
  }, [callOnce, editedUsername, saveUser, token, userData]);

  const deleteUser = useCallback(
    (data) => {
      navigate("/");
      dispatch(actions.session.deleteSession());
      dispatch(actions.user.deleteItem());
    },
    [dispatch, navigate]
  );
  const handleDelete = useCallback(() => {
    if (userData && token)
      callOnce("DELETE", `/users/${userData._id}`, {
        callback: deleteUser,
        config: {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        },
      });
  }, [callOnce, deleteUser, token, userData]);

  const editUsername = useCallback((value: string) => {
    setUsername(value);
  }, []);

  const setEditMode = useCallback((value: boolean) => {
    setEdit(value);
  }, []);

  useEffect(() => {
    if (userData) {
      call("GET", `/users/${userData._id}`, {
        callback: handleGet,
        config: {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        },
      });
    }
    // eslint-disable-next-line
  }, [call, handleGet, token]);
  return {
    userData,
    userInfo,
    edit,
    editedUsername,
    setEditMode,
    editUsername,
    usernameInput,
    handleSave,
    error,
    handleDelete,
  };
};

export default useAccount;
