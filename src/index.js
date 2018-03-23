import hello from 'hellojs';

import './main.css';
import {Main} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

registerServiceWorker();

const validSession = session => {
  const currentTime = new Date().getTime() / 1000;
  return session && session.access_token && session.expires > currentTime;
};

const existingAccessToken = () => {
  const session = hello('github').getAuthResponse();
  return validSession(session) ? session.access_token : null;
};

const flags = {
  accessToken: existingAccessToken(),
};
const app = Main.embed(document.getElementById('root'), flags);

window.hello = hello;

let accessToken, redirectUri;
// if (process.env.NODE_ENV == 'development') {
accessToken = '55f0486a9967a2ac5715';
redirectUri = 'http://lvh.me:3002';
// } else {
//   accessToken = '143fcf6817394a7cf33f'
//   redirectUri = 'https://elm-explorer.netlify.com'
// }

hello.init({github: accessToken}, {redirect_uri: redirectUri});

hello.on(
  'auth.login',
  auth => {
    app.ports.githubOauthSuccess.send(auth.authResponse.access_token);
    console.log('Authenticated!', auth);
  },
  error => console.log('Something went wrong:', error),
);

// XXX Handle logout with port message similar to login, then handle this in
// Elm, so page updates without refresh on logout.
hello.on(
  'auth.logout',
  auth => console.log('Logged out!', auth),
  error => console.log('Something went wrong:', error),
);
