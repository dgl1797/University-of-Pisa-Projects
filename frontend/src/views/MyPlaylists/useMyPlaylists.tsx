import { useCallback, useEffect, useRef, useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import { useNavigate } from "react-router-dom";
import { useAPIs } from "../../hooks";
import { actions, selectors } from "../../store/reducers";

enum tabs {
  filters = 1,
  suggestions = 2,
}

const useMyPlaylists = () => {
  const dispatch = useDispatch();
  const navigate = useNavigate();
  const [page, setpage] = useState(0);
  const userData = useSelector(selectors.user.getUserData);
  const sessionToken = useSelector(selectors.session.getSessionToken);
  const [errorMessage, setErrorMessage] = useState<string>("");

  const title = useRef<HTMLInputElement>(null);
  const min = useRef<HTMLInputElement>(null);
  const max = useRef<HTMLInputElement>(null);
  const year = useRef<HTMLInputElement>(null);
  const [likes, setlikes] = useState<-1 | 1 | undefined>(undefined);
  const [avgDuration, setavgDuration] = useState<-1 | 1 | undefined>(undefined);
  const [creationDate, setcreationDate] = useState<-1 | 1 | undefined>(undefined);
  const [isPublic, setIsPublic] = useState<boolean | undefined>(undefined);
  const [modalOpen, setModalOpen] = useState<boolean>(false);
  const { callOnce, error, call } = useAPIs();
  const playlistList = useSelector(selectors.playlistList.getPrivateList);

  const handleRequest = useCallback(
    (data) => {
      dispatch(actions.playlistList.setPrivateList(data.reply));
    },
    [dispatch]
  );

  useEffect(() => {
    call("GET", "/playlists", {
      callback: handleRequest,
      config: {
        params: {
          q: { isFilter: true },
          page,
          publicQuery: {
            isMine: true,
            isPublic: undefined,
            username: userData?.username,
          },
        },
        headers: {
          Authorization: `Bearer ${sessionToken}`,
        },
      },
    });
    // eslint-disable-next-line
  }, [handleRequest, userData, call, sessionToken]);

  const handleSearch = useCallback(
    (newPage?: number) => {
      let querySchema: any = {};
      querySchema.isFilter = true;
      const avgd =
        !min?.current?.value || !max?.current?.value
          ? undefined
          : {
              low: parseInt(min?.current?.value) ?? undefined,
              high: parseInt(max?.current?.value) ?? undefined,
            };
      querySchema.averageDuration = avgd;
      const tmpYear = parseInt(year.current?.value ?? ""); //vedere per stringhe vuote
      querySchema.year = !!tmpYear || tmpYear === 0 ? tmpYear : undefined;
      const tmpTitle = title?.current?.value ?? undefined;
      querySchema.title = !!tmpTitle ? tmpTitle : undefined;
      let sortSchema: any = {
        likes,
        avgDuration,
        creationDate,
      };
      const publicQuerySchema = {
        isMine: true,
        isPublic,
        username: userData?.username,
      };
      callOnce("GET", "/playlists", {
        callback: handleRequest,
        config: {
          params: {
            q: querySchema,
            s: sortSchema,
            publicQuery: publicQuerySchema,
            page: newPage ?? page,
          },
          headers: {
            Authorization: `Bearer ${sessionToken}`,
          },
        },
      });
    },
    [
      avgDuration,
      callOnce,
      creationDate,
      handleRequest,
      likes,
      page,
      userData,
      sessionToken,
      isPublic,
    ]
  );

  const checkOrderingFields = useCallback(
    (likes?: -1 | 1, avgDuration?: -1 | 1, creationDate?: -1 | 1) => {
      setavgDuration(avgDuration);
      setcreationDate(creationDate);
      setlikes(likes);
    },
    [setavgDuration, setcreationDate, setlikes]
  );
  const handleCheckDurations = useCallback(() => {
    if ((min?.current?.value ?? 0) >= (max?.current?.value ?? 1)) {
      setErrorMessage("invalid duration");
      setTimeout(() => setErrorMessage(""), 3000);
    }
  }, [min, max]);

  const updatePage = useCallback(
    (newPage: number) => {
      if (newPage >= 0) {
        setpage(newPage);
        handleSearch(newPage);
      }
    },
    [setpage, handleSearch]
  );

  const updatePublic = useCallback(() => {
    if (isPublic === true) setIsPublic(false);
    if (isPublic === false) setIsPublic(undefined);
    if (isPublic === undefined) setIsPublic(true);
  }, [isPublic]);

  const handleModalState = useCallback(
    (newState: boolean) => {
      setModalOpen(newState);
    },
    [setModalOpen]
  );

  const handleInsertion = useCallback(
    (data) => {
      dispatch(actions.playlistList.addToPrivateList(data.reply));
      setModalOpen(false);
    },
    [dispatch]
  );

  const handleModalSubmit = useCallback(
    (value: string) => {
      callOnce("POST", "/playlists", {
        callback: handleInsertion,
        payload: {
          title: value,
          author: userData?.username ?? "",
        },
        config: {
          headers: {
            Authorization: `Bearer ${sessionToken}`,
          },
        },
      });
    },
    [callOnce, handleInsertion, sessionToken, userData]
  );

  return {
    title,
    min,
    max,
    year,
    handleCheckDurations,
    errorMessage,
    tabs,
    checkOrderingFields,
    likes,
    avgDuration,
    creationDate,
    handleSearch,
    page,
    updatePage,
    error,
    navigate,
    playlistList,
    updatePublic,
    isPublic,
    modalOpen,
    handleModalState,
    handleModalSubmit,
  };
};

export default useMyPlaylists;
