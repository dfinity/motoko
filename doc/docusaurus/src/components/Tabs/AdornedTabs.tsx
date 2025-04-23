import React, { ReactElement } from "react";
import Tabs, { Props as TabsProps } from "@theme/Tabs";
import { AdornedTabItemProps } from "@site/src/components/Tabs/AdornedTab";

type AdornedTabItem =
  | ReactElement<AdornedTabItemProps>
  | null
  | false
  | undefined;

export interface AdornedTabsProps extends Omit<TabsProps, "children"> {
  readonly children: AdornedTabItem | AdornedTabItem[];
}

export function AdornedTabs(props: AdornedTabsProps) {
  const values = [props.children as React.JSX.Element | React.JSX.Element[]]
    .flatMap((i) => i)
    .map((tabItem) => tabItem.props)
    .map((tabItem) => {
      return {
        ...tabItem,
        default: tabItem.default ?? props.defaultValue === tabItem.value,
        label: (
          <span className={"flex gap-2"}>
            {tabItem.startAdornment}
            {tabItem.label}
            {tabItem.endAdornment}
          </span>
        ),
      };
    });

  return <Tabs className={"ic0-tabs"} {...props} values={values} />;
}
