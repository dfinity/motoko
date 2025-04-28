import React, { ReactNode } from "react";
import TabItem, { Props as TabItemProps } from "@theme/TabItem";

export interface AdornedTabItemProps extends TabItemProps {
  startAdornment?: ReactNode;
  endAdornment?: ReactNode;
}

export function AdornedTab(props: AdornedTabItemProps) {
  return <TabItem {...props} />;
}
