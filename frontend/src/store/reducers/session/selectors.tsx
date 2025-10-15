import { RootState } from "../..";

export const getSessionToken = (state: RootState) => state?.session?.token ?? undefined;
