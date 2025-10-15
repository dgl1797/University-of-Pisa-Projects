import { useCallback, useEffect, useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import { useAPIs } from "../../hooks";
import { IPlaylistRank, IUserRank } from "../../store/models";
import { actions, selectors } from "../../store/reducers";

const useRankings = () => {
  const dispatch = useDispatch();
  const { call } = useAPIs();
  const userData = useSelector(selectors.user.getUserData);
  const [playlistRank, setPlaylistRank] = useState<IPlaylistRank[]>([]);
  const [userRank, setUserRank] = useState<IUserRank[]>([]);
  const handlePlaylistsRequest = useCallback(
    (data) => {
      const newList = data?.reply ?? undefined;
      if (newList) setPlaylistRank(newList);
    },
    [setPlaylistRank]
  );
  const handleUserRequest = useCallback(
    (data) => {
      const newList = data?.reply ?? undefined;
      if (newList) setUserRank(newList);
      else setUserRank(new Array(20).fill({ _id: "", username: "", score: "" }));
    },
    [setUserRank]
  );
  const playlistError = useCallback(
    (error) => {
      setPlaylistRank(new Array(20).fill({ _id: "", author: "", title: "", score: "" }));
    },
    [setPlaylistRank]
  );
  const userError = useCallback(
    (error) => {
      setUserRank(new Array(20).fill({ _id: "", username: "", score: "" }));
    },
    [setUserRank]
  );
  useEffect(() => {
    call("GET", "/playlists/rankings", {
      callback: handlePlaylistsRequest,
      onError: playlistError,
    });
    call("GET", "/users/rankings", {
      callback: handleUserRequest,
      onError: userError,
    });
  }, [call, handlePlaylistsRequest, handleUserRequest, playlistError, userError]);
  const linkClick = useCallback(
    (linkContent: string) => {
      dispatch(actions.playlistFilter.updateAuthorFilter(linkContent));
    },
    [dispatch]
  );
  return { userData, playlistRank, userRank, linkClick };
};

export default useRankings;
