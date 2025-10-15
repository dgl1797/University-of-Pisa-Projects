import { RootState } from "../..";

export const getSongList = (state: RootState) => state?.songList?.list ?? [];
export const getReported = (state: RootState) => state?.songList?.reported ?? [];
export const getProposals = (state: RootState) => state?.songList?.proposals ?? [];
export const getTotalLength = (state: RootState) => state?.songList?.totalLength ?? 0;
