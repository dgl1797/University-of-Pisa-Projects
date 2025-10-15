import { memo, useEffect } from "react";
import { useDispatch, useSelector } from "react-redux";
import { Routes, Route, Navigate } from "react-router-dom";
import { selectors } from "./store/reducers";
import {
  Account,
  Dashboard,
  Home,
  Login,
  MyPlaylists,
  PlaylistsBrowser,
  PlaylistView,
  Rankings,
  RegistrationPage,
  Reports,
} from "./views";

function App() {
  const dispatch = useDispatch();
  const userData = useSelector(selectors.user.getUserData);
  useEffect(() => {
    dispatch({ type: "app/startup" });
  }, [dispatch]);
  return (
    <Routes>
      <Route path="/" element={<Home />} />
      <Route path="/login" element={<Login />} />
      <Route path="/signup" element={<RegistrationPage />} />
      <Route path="/rankings" element={<Rankings />} />
      <Route path="/dashboard" element={<Dashboard />}>
        <Route index element={<Navigate to={`/dashboard/${userData?._id}`} />} />
        <Route path=":userId" element={<Account />} />
        <Route path="rankings" element={<Rankings />} />
        <Route path="playlists" element={<PlaylistsBrowser />} />
        <Route path=":userId/playlists" element={<MyPlaylists />} />
        <Route path=":userId/playlists/:playlistId" element={<PlaylistView isMine />} />
        <Route path="playlists/:playlistId" element={<PlaylistView />} />
        <Route path="reports" element={<Reports />} />
      </Route>
    </Routes>
  );
}

export default memo(App);
