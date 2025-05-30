import styled from "@emotion/styled";

import { color } from "metabase/lib/colors";
import ParameterValueWidget from "metabase/parameters/components/ParameterValueWidget";

export const TagContainer = styled.div`
  padding: 1.5rem 1.5rem 0 1.5rem;
  margin-bottom: 1.5rem;
  border-top: 1px solid ${color("border")};
`;
export const TagName = styled.h3`
  font-weight: 900;
  margin-bottom: 2rem;
  align-self: flex-end;
  color: ${color("brand")};
`;

interface ContainerLabelProps {
  paddingTop?: boolean;
}

export const ContainerLabel = styled.div<ContainerLabelProps>`
  display: block;
  margin-bottom: 0.5em;
  padding-top: ${props => (props.paddingTop ? "0.5rem" : "0")};
  color: ${color("text-medium")};
  font-weight: 700;
`;

export const ErrorSpan = styled.span`
  color: ${color("error")};
`;

interface InputContainerProps {
  lessBottomPadding?: boolean;
}
export const InputContainer = styled.label<InputContainerProps>`
  display: block;
  padding-bottom: ${props => (props.lessBottomPadding ? "1.5rem" : "2rem")};
`;

export const DefaultParameterValueWidget = styled(ParameterValueWidget)`
  padding: 0.5rem;
  font-weight: 700;
  color: ${color("text-medium")};
  border-radius: 0.5rem;
  background-color: ${color("white")};
  border: 2px solid ${color("border")};
`;

export const ToggleContainer = styled.div`
  display: flex;
  gap: 0.5rem;
  margin: 0.5rem 0;
`;

export const ToggleLabel = styled.label`
  display: flex;
  gap: 0.5rem;
  margin-top: 0.35rem;
  font-weight: 700;
  color: ${color("text-medium")};
  cursor: pointer;
`;
