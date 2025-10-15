const formatToTime = (x: number) => {
  return `${x < 10 ? "0" : ""}${x}`;
};

export const secondsToDuration = (time: number) => {
  const hours = Math.floor(time / 3600);
  const remainingSeconds = time - hours * 3600;
  const minutes = Math.floor(remainingSeconds / 60);
  const seconds = remainingSeconds - minutes * 60;
  return `${hours !== 0 ? `${formatToTime(hours)}h ` : ""}${
    minutes !== 0 ? `${formatToTime(minutes)}m ` : ""
  }${seconds !== 0 ? `${formatToTime(seconds)}s` : ""}`;
};
