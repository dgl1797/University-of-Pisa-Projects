import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import playlistIcon from "../../assets/whitePlayIcon.jpg";
import bluePlaylistIcon from "../../assets/bluePlayIcon.png";
import { useNavigate } from "react-router-dom";
import { useDispatch, useSelector } from "react-redux";
import { actions, selectors } from "../../store/reducers";
import { useAPIs } from "../../hooks";

const useDashboard = () => {
  const token = useSelector(selectors.session.getSessionToken);
  const dispatch = useDispatch();
  const { callOnce } = useAPIs();
  const userData = useSelector(selectors.user.getUserData);
  const [noReps, setNoReps] = useState<number>(0);
  const imageRef = useRef<HTMLImageElement>(null);
  const navigator = useNavigate();
  const [isOpen, setIsOpen] = useState<boolean>(false);

  useEffect(() => {
    if (userData && token) {
      if (!userData._id) {
        navigator("/login");
      }
    } else {
      navigator("/login");
    }
  }, [userData, navigator, token]);

  const handleReps = useCallback((data) => {
    setNoReps(data.numberOfReports);
  }, []);

  useEffect(() => {
    callOnce("GET", "/songs/reports/number", {
      callback: handleReps,
      config: {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      },
    });
    // eslint-disable-next-line
  }, [handleReps]);

  const changeImage = useCallback(() => {
    if (imageRef.current) imageRef.current.src = bluePlaylistIcon;
  }, []);
  const returnImage = useCallback(() => {
    if (imageRef.current) imageRef.current.src = playlistIcon;
  }, []);
  const close = useCallback(() => {
    setIsOpen(false);
  }, []);
  const open = useCallback(() => {
    setIsOpen(!isOpen);
  }, [isOpen]);
  const logout = useCallback(() => {
    navigator("/", { replace: true });
    // dispatch will execute after navigating aswell
    dispatch(actions.session.deleteSession());
    dispatch(actions.user.deleteItem());
  }, [navigator, dispatch]);
  const requestLogout = useCallback(() => {
    if (token) {
      callOnce("DELETE", "/auth", {
        callback: logout,
        onError: (error) => {
          logout();
        },
        config: {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        },
      });
    }
  }, [callOnce, logout, token]);
  const navigateToAccountPage = useCallback(() => {
    if (userData && token) {
      navigator(`/dashboard/${userData._id}`);
    }
  }, [navigator, userData, token]);
  const navigateToMyPlaylists = useCallback(() => {
    if (userData && token) {
      navigator(`/dashboard/${userData._id}/playlists`);
    }
  }, [userData, token, navigator]);
  const navigateToReports = useCallback(() => {
    if (userData && token) {
      navigator(`/dashboard/reports`);
    }
  }, [userData, token, navigator]);
  const menuOptions = useMemo(() => {
    const common = [
      {
        key: "ACCOUNT",
        action: navigateToAccountPage,
      },
      { key: "MY PLAYLISTS", action: navigateToMyPlaylists },
    ];
    if (userData?.isValidator) {
      return common.concat({ key: `REPORTS (${noReps})`, action: navigateToReports });
    }
    return common;
  }, [userData, navigateToAccountPage, navigateToMyPlaylists, navigateToReports, noReps]);
  return {
    imageRef,
    changeImage,
    returnImage,
    isOpen,
    close,
    open,
    requestLogout,
    userData,
    navigator,
    menuOptions,
  };
};

export default useDashboard;
