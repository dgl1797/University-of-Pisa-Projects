import { RootState } from "../..";

export const getUserData = (state: RootState) => state?.user?.item ?? undefined;
export const getUserInfo = (state: RootState) => state?.user?.info ?? undefined;
