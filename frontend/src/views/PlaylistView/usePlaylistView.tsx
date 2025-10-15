import moment from "moment";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import { useNavigate, useParams } from "react-router-dom";
import { useAPIs } from "../../hooks";
import { IPlaylist } from "../../store/models";
import { actions, selectors } from "../../store/reducers";

enum orderings {
  DATE = 1,
  DURATION = 2,
}

const usePlaylistView = (isMine?: boolean) => {
  const dispatch = useDispatch();
  const navigate = useNavigate();
  const searchBar = useRef<HTMLInputElement>(null);
  const [searchFilter, setSearchFilter] = useState<string>("");
  const [orderBy, setOrderBy] = useState<{ active?: orderings; ordering?: -1 | 1 }>({});
  const { playlistId } = useParams();
  const userData = useSelector(selectors.user.getUserData);
  const sessionToken = useSelector(selectors.session.getSessionToken);
  const playlistInfo = useSelector(selectors.playlistList.getPlaylist);
  const [selectedSongs, setSelectedSongs] = useState<string[]>([]);
  const [selectedLinks, setSelectedLinks] = useState<string[]>([]);
  const [isOpen, setIsOpen] = useState<boolean>(false);
  const [successMessage, setSuccessMessage] = useState<string | undefined>(undefined);
  const { call, callOnce, isLoading, error } = useAPIs();

  const handleRequest = useCallback(
    (data) => {
      dispatch(actions.playlistList.setPlaylistInfo(data.reply));
    },
    [dispatch]
  );

  const filteredSongs = useMemo(() => {
    return playlistInfo?.songs
      ?.filter(
        (s) =>
          s?.artist?.toLowerCase()?.includes(searchFilter?.toLowerCase()) ||
          s?.title?.toLowerCase()?.includes(searchFilter?.toLowerCase())
      )
      .sort((s1, s2) => {
        if (orderBy.active === orderings.DATE && orderBy.ordering) {
          return orderBy.ordering * moment(s1.insertionDate).diff(moment(s2.insertionDate));
        } else if (orderBy.active === orderings.DURATION && orderBy.ordering) {
          return orderBy.ordering * (s1.duration - s2.duration);
        }
        return 1;
      });
  }, [searchFilter, orderBy, playlistInfo]);

  const handleSelect = useCallback(
    (checked: boolean, newSelection: { _id: string; link: string }) => {
      if (checked) {
        setSelectedSongs(selectedSongs.concat(newSelection._id));
        setSelectedLinks(selectedLinks.concat(newSelection.link));
      } else {
        setSelectedSongs(selectedSongs.filter((s) => s !== newSelection._id));
        setSelectedLinks(selectedLinks.filter((l) => l !== newSelection.link));
      }
    },
    [selectedSongs, selectedLinks]
  );

  const selectAll = useCallback(
    (checked: boolean) => {
      if (checked) {
        setSelectedSongs(filteredSongs?.map((s) => s._id) ?? []);
        setSelectedLinks(filteredSongs?.map((s) => s.link) ?? []);
      } else {
        setSelectedSongs([]);
        setSelectedLinks([]);
      }
    },
    [filteredSongs]
  );

  useEffect(() => {
    if (!isMine) {
      if (userData?.username === playlistInfo?.author) {
        navigate(`/dashboard/${userData?._id}/playlists/${playlistId}`, { replace: true });
      }
    }
    if (isMine) {
      if (userData?.username !== playlistInfo?.author) {
        navigate(`/dashboard/playlists/${playlistId}`, { replace: true });
      }
    }
  }, [isMine, navigate, userData, playlistId, playlistInfo]);

  useEffect(() => {
    call("GET", `/playlists/${playlistId}`, {
      callback: handleRequest,
      config: {
        headers: {
          Authorization: `Bearer ${sessionToken}`,
        },
      },
    });
  }, [call, handleRequest, playlistId, sessionToken, userData]);

  const handleActivation = useCallback((newValue?: orderings) => {
    setOrderBy({
      active: newValue,
      ordering: 1,
    });
  }, []);

  const handleOrdering = useCallback(() => {
    if (orderBy.ordering === 1) setOrderBy({ ...orderBy, ordering: -1 });
    if (orderBy.ordering === -1) setOrderBy({ ...orderBy, ordering: undefined });
    if (orderBy.ordering === undefined) setOrderBy({ ...orderBy, ordering: 1 });
  }, [orderBy]);

  const handleSearch = useCallback(() => {
    setSearchFilter(searchBar.current?.value ?? "");
  }, []);

  const handleDeletionSuccess = useCallback(
    (data) => {
      setSelectedSongs([]);
      setSelectedLinks([]);
      dispatch(actions.playlistList.updatePlaylistInfo(data.reply));
    },
    [dispatch]
  );

  const handleSuccess = useCallback((data) => {
    setSuccessMessage(data.message);
    setTimeout(() => setSuccessMessage(undefined), 3000);
  }, []);
  const handleReport = useCallback(() => {
    callOnce("POST", "/songs/report", {
      callback: handleSuccess,
      payload: { repLinks: selectedLinks },
      config: {
        headers: {
          Authorization: `Bearer ${sessionToken}`,
        },
      },
    });
  }, [callOnce, handleSuccess, sessionToken, selectedLinks]);

  const deletionSuccess = useCallback(() => {
    navigate(`/dashboard/${userData?._id}/playlists`);
  }, [navigate, userData]);

  const deletePlaylist = useCallback(() => {
    callOnce("DELETE", `/playlists/${playlistId}`, {
      callback: deletionSuccess,
      config: {
        headers: {
          Authorization: `Bearer ${sessionToken}`,
        },
      },
    });
  }, [callOnce, sessionToken, playlistId, deletionSuccess]);

  const likedSuccess = useCallback((data) => {
    window.location.reload();
  }, []);
  const postLike = useCallback(() => {
    callOnce("POST", `/playlists/${playlistId}/like`, {
      callback: likedSuccess,
      config: {
        headers: {
          Authorization: `Bearer ${sessionToken}`,
        },
      },
    });
  }, [callOnce, likedSuccess, playlistId, sessionToken]);

  const handleDeletion = useCallback(() => {
    callOnce("PUT", `/playlists/${playlistId}`, {
      callback: handleDeletionSuccess,
      payload: { songsList: selectedSongs },
      config: {
        params: { type: "remove" },
        headers: {
          Authorization: `Bearer ${sessionToken}`,
        },
      },
    });
  }, [callOnce, handleDeletionSuccess, playlistId, selectedSongs, sessionToken]);

  const handlePublish = useCallback(
    (data) => {
      dispatch(
        actions.playlistList.updatePlaylistInfo({
          ...playlistInfo,
          isPublic: true,
        } as IPlaylist)
      );
    },
    [playlistInfo, dispatch]
  );

  const publish = useCallback(() => {
    callOnce("PUT", `/playlists/${playlistId}/publish`, {
      callback: handlePublish,
      config: {
        headers: {
          Authorization: `Bearer ${sessionToken}`,
        },
      },
    });
  }, [callOnce, playlistId, sessionToken, handlePublish]);

  const handleRedirect = useCallback((link: string) => {
    window.open(link, "_blank");
  }, []);

  return {
    searchBar,
    filteredSongs,
    handleSearch,
    handleActivation,
    handleOrdering,
    orderBy,
    handleRedirect,
    playlistInfo,
    handleSelect,
    selectedSongs,
    selectAll,
    isOpen,
    setIsOpen,
    handleDeletion,
    isLoading,
    error,
    handleReport,
    successMessage,
    deletePlaylist,
    postLike,
    publish,
  };
};

export default usePlaylistView;
