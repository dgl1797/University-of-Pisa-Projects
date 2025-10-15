import { useCallback, useEffect, useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import { useAPIs } from "../../../hooks";
import { actions, selectors } from "../../../store/reducers";

const usePlaylistModal = (onClose: () => void) => {
  const dispatch = useDispatch();
  const { error, callOnce, call } = useAPIs();
  const [successMessage, setSuccessMessage] = useState<string | undefined>(undefined);
  const userData = useSelector(selectors.user.getUserData);
  const sessionToken = useSelector(selectors.session.getSessionToken);
  const personalPlaylists = useSelector(selectors.playlistList.getSelectableList);

  const handleRequest = useCallback(
    (data) => {
      dispatch(actions.playlistList.setSelectablePlaylists(data.reply));
    },
    [dispatch]
  );
  useEffect(() => {
    call("GET", `/playlists/getAll/${userData?._id}`, {
      callback: handleRequest,
      config: {
        headers: {
          Authorization: `Bearer ${sessionToken}`,
        },
      },
    });
  }, [handleRequest, userData, call, sessionToken]);

  const handleMerge = useCallback(
    (data) => {
      dispatch(actions.playlistList.updatePlaylistInfo(data.reply));
      setSuccessMessage("sucessfully added selected songs");
      onClose();
    },
    [dispatch, onClose]
  );
  const mergeSongs = useCallback(
    (playlistId: string, selectedSongs: string[]) => {
      callOnce("PUT", `/playlists/${playlistId}`, {
        callback: handleMerge,
        payload: { songsList: selectedSongs },
        config: {
          params: { type: "add" },
          headers: {
            Authorization: `Bearer ${sessionToken}`,
          },
        },
      });
    },
    [callOnce, sessionToken, handleMerge]
  );

  return { error, successMessage, personalPlaylists, mergeSongs };
};

export default usePlaylistModal;
