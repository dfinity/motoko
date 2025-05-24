import useIsBrowser from "@docusaurus/useIsBrowser";

export enum OsType {
  Linux = "Linux",
  macOs = "macOS",
  Windows = "Windows",
}

export function useOs(): { current: OsType } {
  const isBrowser = useIsBrowser();

  const getOS = () => {
    let os: OsType | null = null;

    if (isBrowser) {
      if (window.navigator.userAgent.indexOf("Windows") != -1) {
        os = OsType.Windows;
      } else if (window.navigator.userAgent.indexOf("Mac OS") != -1) {
        os = OsType.macOs;
      } else if (window.navigator.userAgent.indexOf("Linux") != -1) {
        os = OsType.Linux;
      }
    }

    return os;
  };

  return {
    current: getOS(),
  };
}
