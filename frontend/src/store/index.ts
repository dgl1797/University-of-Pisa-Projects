import { combineReducers, createStore } from "redux";
import { reducers } from "./reducers";

const loadState = () => {
  return JSON.parse(localStorage?.getItem("redux") ?? "{}");
};

const rootReducer = combineReducers({ ...reducers });

const store = createStore(
  rootReducer,
  loadState(),
  (window as any).__REDUX_DEVTOOLS_EXTENSION__ && (window as any).__REDUX_DEVTOOLS_EXTENSION__()
);
export type RootState = ReturnType<typeof rootReducer>;

const saveState = (state: RootState) => {
  return localStorage.setItem("redux", JSON.stringify(state));
};
store.subscribe(() => saveState(store.getState()));

export default store;
