// @flow
import React from "react";

/* Additional TS types
 */
type EmailLocaleIDs = "welcome_email" | "email_heading";
type FooterLocaleIDs = "footer_title" | "footer_sendoff";

type AllLocaleIDs = `${EmailLocaleIDs | FooterLocaleIDs}_id`;

function Counter() {
  const [count, setCount] = React.useState(0);
  const [count2, setCount2] = React.useState(() => 0);
  return (
    <div>
      <button onClick={() => setCount((c) => c + 1)}>Increment</button>
      <button onClick={() => setCount2(count2 + 1)}>Increment2</button>
      <div>{count}</div>
      <div>{count2}</div>
    </div>
  );
}

// Additional TS types
export default function App({
  text = "Hello",
  onClick,
}: {
  text: string;
  onClick: () => void;
}) {
  return (
    <>
      <Counter />
      <div onClick={onClick}>{text}</div>
    </>
  );
}
