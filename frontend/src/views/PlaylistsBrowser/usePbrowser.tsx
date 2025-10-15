import { useCallback, useEffect, useRef, useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import { useNavigate } from "react-router-dom";
import { useAPIs } from "../../hooks";
import { actions, selectors } from "../../store/reducers";

enum tabs {
  filters = 1,
  suggestions = 2,
}

const usePbrowser = () => {
  const dispatch = useDispatch();
  const navigate = useNavigate();
  const [selectedTab, setselectedTab] = useState(tabs.filters);
  const [page, setpage] = useState(0);
  const [errorMessage, setErrorMessage] = useState<string>("");
  const [liked, setLiked] = useState<boolean>(false);
  const authorFilterDefault = useSelector(selectors.playlistFilter.getAuthor);
  const userData = useSelector(selectors.user.getUserData);
  const sessionToken = useSelector(selectors.session.getSessionToken);

  const authorName = useRef<HTMLInputElement>(null);
  const title = useRef<HTMLInputElement>(null);
  const min = useRef<HTMLInputElement>(null);
  const max = useRef<HTMLInputElement>(null);
  const year = useRef<HTMLInputElement>(null);
  const [likes, setlikes] = useState<-1 | 1 | undefined>(undefined);
  const [avgDuration, setavgDuration] = useState<-1 | 1 | undefined>(undefined);
  const [creationDate, setcreationDate] = useState<-1 | 1 | undefined>(undefined);
  const [activeSuggestion, setActiveSuggestion] = useState<1 | 2>(1); //1 clone hops,2 like hops
  const { callOnce, error, call } = useAPIs();
  const playlistList = useSelector(selectors.playlistList.getPublicList);

  const handleRequest = useCallback(
    (data) => {
      dispatch(actions.playlistList.setPublicList(data.reply));
    },
    [dispatch]
  );

  useEffect(() => {
    call("GET", "/playlists", {
      callback: handleRequest,
      config: {
        params: {
          q: { isFilter: true, author: !!authorFilterDefault ? authorFilterDefault : undefined },
          page,
          publicQuery: {
            isMine: false,
            isPublic: true,
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
      if (selectedTab === 2) {
        querySchema.cloneHops = activeSuggestion === 1 ? true : undefined;
        querySchema.likeHops = activeSuggestion === 2 ? true : undefined;
      } else {
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
        querySchema.author = !!authorFilterDefault ? authorFilterDefault : undefined;
        const tmpTitle = title?.current?.value ?? undefined;
        querySchema.title = !!tmpTitle ? tmpTitle : undefined;
        querySchema.liked = liked;
      }
      //if (Object.values(querySchema).every((el) => el === undefined)) querySchema = undefined;
      let sortSchema: any = {
        likes,
        avgDuration,
        creationDate,
      };
      //if (Object.values(sortSchema).every((el) => el === undefined)) sortSchema = undefined;
      const publicQuerySchema = {
        isMine: false,
        isPublic: true,
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
      activeSuggestion,
      authorFilterDefault,
      avgDuration,
      callOnce,
      creationDate,
      handleRequest,
      liked,
      likes,
      page,
      selectedTab,
      userData,
      sessionToken,
    ]
  );

  const handleSuggestionChange = useCallback((suggestion: 1 | 2) => {
    setActiveSuggestion(suggestion);
  }, []);

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
  const saveAuthor = useCallback(() => {
    dispatch(actions.playlistFilter.updateAuthorFilter(authorName.current?.value ?? ""));
  }, [authorName, dispatch]);
  const changeTab = useCallback(
    (tab: number) => {
      setselectedTab(tab);
    },
    [setselectedTab]
  );
  const updatePage = useCallback(
    (newPage: number) => {
      if (newPage >= 0) {
        setpage(newPage);
        handleSearch(newPage);
      }
    },
    [setpage, handleSearch]
  );
  const updateSetLiked = useCallback(
    (liked: boolean) => {
      setLiked(liked);
    },
    [setLiked]
  );
  return {
    authorName,
    title,
    min,
    max,
    year,
    selectedTab,
    handleCheckDurations,
    changeTab,
    updateSetLiked,
    liked,
    saveAuthor,
    errorMessage,
    tabs,
    checkOrderingFields,
    likes,
    avgDuration,
    creationDate,
    handleSuggestionChange,
    activeSuggestion,
    authorFilterDefault,
    handleSearch,
    page,
    updatePage,
    error,
    navigate,
    playlistList,
  };
};

export default usePbrowser;
