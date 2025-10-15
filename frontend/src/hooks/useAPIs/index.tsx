// eslint-disable-next-line @typescript-eslint/no-unused-vars
import axios, { Axios, AxiosError, AxiosRequestConfig, Canceler } from "axios";
import { useCallback, useMemo, useState } from "react";

const useAPIs = (hostname?: string, hostPort?: string) => {
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [error, setError] = useState<string>("");
  const [success, setSuccess] = useState<string>("");
  const [aborter, setAborter] = useState<AbortController | undefined>(undefined);
  if (!hostname) hostname = process?.env?.REACT_APP_SERVER_HOST ?? "localhost";
  if (!hostPort) hostPort = process?.env?.REACT_APP_SERVER_PORT ?? "5000";

  const url = useMemo(() => `http://${hostname}:${hostPort}`, [hostPort, hostname]);

  const delay = (milliseconds: number) =>
    new Promise((resolve) => setTimeout(resolve, milliseconds));

  const call = useCallback(
    async (
      method: "GET" | "POST" | "PUT" | "DELETE",
      uri: string,
      request: {
        callback?: (data: any, args?: { [key: string]: any }) => any;
        cbArguments?: { [key: string]: any };
        onError?: (error: any) => void;
        payload?: any;
        successMessage?: string;
        config?: AxiosRequestConfig<any> | undefined;
        cancellable?: boolean;
      } = {}
    ) => {
      try {
        setIsLoading(true);
        const { data } =
          method === "GET"
            ? await axios.get(url + uri, request.config)
            : method === "DELETE"
            ? await axios.delete(url + uri, request.config)
            : method === "POST"
            ? await axios.post(url + uri, request?.payload, request.config)
            : await axios.put(url + uri, request?.payload, request.config);
        setIsLoading(false);
        if (request?.callback) {
          setAborter(undefined);
          return request.callback(data, request?.cbArguments);
        }
        setSuccess(request.successMessage ?? "Completed!");
        delay(3000).then(() => setSuccess(""));
        setAborter(undefined);
        return data;
      } catch (err: AxiosError | any) {
        setIsLoading(false);
        if (axios.isCancel(err) && request.cancellable) return;
        if (err?.response) {
          const message = `ERROR ${" " + err.response?.status ?? ""}${
            ": " + err.response?.data.message ?? ""
          }`;
          setError(message);
          if (request?.onError) request.onError(err);
          delay(3000).then(() => setError(""));
        } else if (err?.request) {
          const message = `ERROR: ${err.message}`;
          setError(message);
          if (request?.onError) request.onError(err);
          delay(3000).then(() => setError(""));
        } else {
          setError(`ERROR: ${err.message}`);
          if (request?.onError) request.onError(err);
          delay(3000).then(() => setError(""));
        }
      }
    },
    [url]
  );

  const callOnce = useCallback(
    async (
      method: "GET" | "POST" | "PUT" | "DELETE",
      uri: string,
      request?: {
        callback?: (data: any, args?: { [key: string]: any }) => any;
        cbArguments?: { [key: string]: any };
        onError?: (error) => void;
        payload?: any;
        successMessage?: string;
        config?: AxiosRequestConfig<any> | undefined;
      }
    ) => {
      if (isLoading) return;
      return await call(method, uri, request);
    },
    [call, isLoading]
  );

  const callLast = useCallback(
    async (
      method: "GET" | "POST" | "PUT" | "DELETE",
      uri: string,
      request?: {
        callback?: (data: any, args?: { [key: string]: any }) => any;
        cbArguments?: { [key: string]: any };
        onError?: (error) => void;
        payload?: any;
        successMessage?: string;
        config?: AxiosRequestConfig<any> | undefined;
      }
    ) => {
      if (aborter !== undefined) aborter.abort();
      const newAborter = new AbortController();
      setAborter(newAborter);
      return await call(method, uri, {
        ...request,
        config: {
          ...request?.config,
          signal: newAborter.signal,
        },
        cancellable: true,
      });
    },
    [call, aborter]
  );

  return {
    isLoading,
    error,
    success,
    call,
    callOnce,
    callLast,
    delay,
  };
};

export default useAPIs;
