import { RootState } from "../..";

export const getAuthor = (state: RootState) => state?.playlistFilter?.authorFilter ?? undefined;
//export const getUnpersistentCounter = (state: RootState) =>
//  state?.playlistFilter?.unpersistentCounter ?? 0;
