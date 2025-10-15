import { RootState } from "../..";

export const getAppInfo = (state: RootState) => state?.app?.info ?? {};
