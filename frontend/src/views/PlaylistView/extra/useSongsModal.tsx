import { useCallback, useMemo, useRef, useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import { useParams } from "react-router-dom";
import { useAPIs } from "../../../hooks";
import { actions, selectors } from "../../../store/reducers";

const useSongsModal = (onClose: () => void) => {
  const { callLast, callOnce, isLoading, error } = useAPIs();
  const dispatch = useDispatch();
  const sessionToken = useSelector(selectors.session.getSessionToken);
  const songsList = useSelector(selectors.songList.getSongList);
  const { playlistId } = useParams();
  const [selectedSongs, setSelectedSongs] = useState<string[]>([]);
  const titleRef = useRef<HTMLInputElement>(null);
  const artistRef = useRef<HTMLInputElement>(null);
  const proposedLink = useRef<HTMLInputElement>(null);
  const [nose, setNose] = useState<boolean>(false);
  const [proposalBar, setProposalBar] = useState<boolean>(true);
  const loaderObserver = useRef<any>(null);
  const totalLength = useSelector(selectors.songList.getTotalLength);
  const [page, setPage] = useState<number>(0);
  const [successMessage, setSuccessMessage] = useState<string | undefined>(undefined);

  const hasMore = useMemo(() => {
    return totalLength > songsList.length;
  }, [songsList, totalLength]);

  const handleRequest = useCallback(
    (data) => {
      dispatch(actions.songList.setList(data.reply));
      dispatch(actions.songList.setTotalLength(data.totalLength));
      setPage(0);
    },
    [dispatch]
  );

  const handlePageChange = useCallback(
    (data) => {
      dispatch(actions.songList.addToList(data.reply));
      dispatch(actions.songList.setTotalLength(data.totalLength));
    },
    [dispatch]
  );

  const queryPage = useCallback(
    (newPage?: number) => {
      const queryStrings = {
        title: titleRef.current?.value?.replace(/\s+/g, " ")?.trim() ?? undefined,
        artist: artistRef.current?.value?.replace(/\s+/g, " ")?.trim() ?? undefined,
      };
      callLast("GET", "/songs", {
        callback: handlePageChange,
        config: {
          params: {
            q: queryStrings,
            page: newPage ?? page,
          },
          headers: {
            Authorization: `Bearer ${sessionToken}`,
          },
        },
      });
    },
    [callLast, handlePageChange, page, sessionToken]
  );

  const handleSongsAdd = useCallback(
    (data) => {
      dispatch(actions.playlistList.updatePlaylistInfo(data.reply));
      setSelectedSongs([]);
      onClose();
    },
    [dispatch, onClose]
  );

  const handleConfirmation = useCallback(() => {
    callOnce("PUT", `/playlists/${playlistId}`, {
      callback: handleSongsAdd,
      payload: { songsList: selectedSongs },
      config: {
        params: { type: "add" },
        headers: {
          Authorization: `Bearer ${sessionToken}`,
        },
      },
    });
  }, [callOnce, handleSongsAdd, playlistId, selectedSongs, sessionToken]);

  const handleDynamicSearch = useCallback(() => {
    setNose(false);
    const queryStrings = {
      title: titleRef.current?.value?.replace(/\s+/g, " ")?.trim() ?? undefined,
      artist: artistRef.current?.value?.replace(/\s+/g, " ")?.trim() ?? undefined,
    };
    setPage(0);
    callLast("GET", "/songs", {
      callback: handleRequest,
      config: {
        params: {
          q: queryStrings,
          page: 0,
        },
        headers: {
          Authorization: `Bearer ${sessionToken}`,
        },
      },
    });
  }, [handleRequest, callLast, sessionToken]);

  const handleSelection = useCallback(
    (checked: boolean, id: string) => {
      if (checked) {
        setSelectedSongs(selectedSongs.concat(id));
      } else {
        setSelectedSongs(selectedSongs.filter((s) => s !== id));
      }
    },
    [selectedSongs]
  );

  const viewProposalBar = useCallback(() => {
    if (artistRef.current?.value === "" && titleRef.current?.value === "") {
      setProposalBar(true);
    } else {
      setProposalBar(false);
    }
  }, []);

  const selectAll = useCallback(
    (checked: boolean) => {
      if (checked) {
        if (artistRef.current?.value === "" && titleRef.current?.value === "") {
          setNose(true);
        }
        setSelectedSongs(songsList.map((s) => s._id.toString()));
      } else {
        if (artistRef.current?.value === "" && titleRef.current?.value === "") {
          setNose(false);
        }
        setSelectedSongs([]);
      }
    },
    [songsList]
  );

  const handleReply = useCallback(
    (data) => {
      if (data.message) {
        onClose();
        setSuccessMessage(data.message);
        setTimeout(() => setSuccessMessage(undefined), 3000);
      } else {
        if (data.reply) {
          dispatch(actions.songList.setList(data.reply));
        }
      }
    },
    [dispatch, onClose]
  );
  const handleProposal = useCallback(() => {
    callOnce("POST", "/songs/propose", {
      callback: handleReply,
      config: {
        params: { pl: proposedLink.current?.value ?? "" },
        headers: {
          Authorization: `Bearer ${sessionToken}`,
        },
      },
    });
  }, [callOnce, sessionToken, handleReply]);

  const loaderRef = useCallback(
    (node) => {
      if (isLoading) return;
      if (loaderObserver.current) loaderObserver.current.disconnect();
      loaderObserver.current = new IntersectionObserver((entries) => {
        if (entries[0].isIntersecting) {
          setPage(page + 1);
          queryPage(page + 1);
        }
      });
      if (node) loaderObserver.current.observe(node);
    },
    [page, isLoading, queryPage]
  );

  return {
    handleDynamicSearch,
    titleRef,
    artistRef,
    songsList,
    hasMore,
    loaderRef,
    selectAll,
    selectedSongs,
    handleSelection,
    nose,
    isLoading,
    handleConfirmation,
    error,
    proposedLink,
    viewProposalBar,
    proposalBar,
    handleProposal,
    successMessage,
  };
};

export default useSongsModal;
