// @flow

type EmailLocaleIDs = "welcome_email" | "email_heading";
type FooterLocaleIDs = "footer_title" | "footer_sendoff";

type AllLocaleIDs = `${EmailLocaleIDs | FooterLocaleIDs}_id`;

export default function HelloWorld({
  text = "Hello",
  onClick,
}: {
  text: string;
  onClick: () => void;
}) {
  return <div onClick={onClick}>{text}</div>;
}
